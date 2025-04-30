#' Translate Texts Using the DeepL API
#'
#' This function sends text to DeepL's translation API and returns the
#' translated results. It handles batching of large text inputs and parallel
#' processing of translation requests.
#'
#' @param data Character scalar or vector to be translated. Required.
#' @param target_lang Character scalar specifying the target language. Must be
#' one of the supported DeepL language codes listed in
#' [VALID_TRANSLATE_TARGET_LANGUAGES]. Required.
#' @param source_lang Character scalar specifying the source language
#' (optional). If NULL (default), DeepL will auto-detect the source language.
#' Must be one of the supported DeepL language codes listed in
#' [VALID_TRANSLATE_SOURCE_LANGUAGES].
#' @param auth_key Character scalar containing the DeepL API authentication key.
#'        Required. Can be provided directly or set as environment variable
#'        'DEEPL_AUTH_KEY'.
#' @param formality Character scalar specifying the desired formality level
#'        (optional). Must be one of: "default", "more", "less", "prefer_more",
#'        "prefer_less".
#' @param max_request_size Numeric scalar specifying the maximum request size in
#'        kilobytes (default: 80). DeepL limits individual requests to
#'        approximately 128KiB.
#' @param timeout Numeric scalar specifying the timeout for API requests in
#'        seconds (default: 30).
#'
#' @seealso [VALID_TRANSLATE_SOURCE_LANGUAGES],
#' [VALID_TRANSLATE_TARGET_LANGUAGES]
#'
#' @return Character vector of translated text. Will preserve the length and
#'         order of the input. NA values in the input will remain NA in the
#'         output.
#'
#' @examples
#' \dontrun{
#' # Simple translation
#' translate("Hello world", target_lang = "DE", auth_key = "your-api-key")
#'
#' # Translate multiple strings with specified source language
#' texts <- c("Hello", "How are you?", "Thank you")
#' translate(texts,
#'   target_lang = "FR", source_lang = "EN",
#'   auth_key = "your-api-key"
#' )
#'
#' # Using an environment variable for authentication
#' Sys.setenv(DEEPL_AUTH_KEY = "your-api-key")
#' translate("Hello world", target_lang = "ES")
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_form req_perform_parallel
#' @importFrom httr2 resp_body_json req_timeout req_retry resp_status
#' @importFrom purrr map list_rbind
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else across pull everything
translate <- function(
  data,
  target_lang,
  source_lang = NULL,
  auth_key = Sys.getenv("DEEPL_AUTH_KEY"),
  formality = "default",
  max_request_size = 80,
  timeout = 30
) {
  # Input validation
  if (missing(data) || !is.character(data)) {
    stop("'data' must be a character vector")
  }

  # Validate target_lang
  if (
    missing(target_lang) ||
      !target_lang %in% VALID_TRANSLATE_TARGET_LANGUAGES
  ) {
    stop(
      "'target_lang' must be one of: ",
      paste(VALID_TRANSLATE_TARGET_LANGUAGES, collapse = ", ")
    )
  }

  # Validate source_lang if provided
  if (
    !is.null(source_lang) &&
      !source_lang %in% VALID_TRANSLATE_SOURCE_LANGUAGES
  ) {
    stop(
      "'source_lang' must be one of: ",
      paste(VALID_TRANSLATE_SOURCE_LANGUAGES, collapse = ", ")
    )
  }

  # Validate auth_key
  if (auth_key == "" || !is.character(auth_key) || length(auth_key) != 1) {
    stop(
      "'auth_key' must be provided either directly or via the ",
      "DEEPL_AUTH_KEY environment variable"
    )
  }

  # Validate formality
  valid_formality <- c("default", "more", "less", "prefer_more", "prefer_less")
  if (!formality %in% valid_formality) {
    stop(
      "'formality' must be one of: ",
      paste(valid_formality, collapse = ", ")
    )
  }

  # Determine API endpoint based on the key (free vs pro)
  api_url <- if (grepl("^[a-zA-Z0-9]+:fx", auth_key)) {
    "https://api-free.deepl.com/v2/translate"
  } else {
    "https://api.deepl.com/v2/translate"
  }

  # Store original NA positions to restore later
  na_positions <- is.na(data)

  # convert NA to "NA" to allow for accurate size computation
  data[na_positions] <- "NA"

  # Set up the key
  key <- paste0("DeepL-Auth-Key ", auth_key)

  # Construct base request with retry logic
  req_base <-
    request(api_url) |>
    req_headers(Authorization = key, Accept = "application/json") |>
    req_body_form(target_lang = target_lang) |>
    req_timeout(timeout) |>
    # Retry on transient errors (5xx, 429) up to 3 times with backoff
    req_retry(
      max_tries = 3,
      is_transient = ~ resp_status(.x) %in% c(429, 500, 502, 503, 504)
    )

  # Add source_lang if specified
  if (!is.null(source_lang)) {
    req_base <- req_body_form(req_base, source_lang = source_lang)
  }

  # Add formality if not default
  if (formality != "default") {
    req_base <- req_body_form(req_base, formality = formality)
  }

  # Prepare batches for parallel processing
  batches <- data |>
    split_into_list(max_kib = max_request_size) |>
    map(\(x) req_body_form(req_base, text = x, .multi = "explode"))

  # Execute translation requests in parallel batches
  results <- req_perform_parallel(
    batches,
    progress = TRUE,
    on_error = "continue" # Continue even if a request fails after retries
  )

  # Process results: Handle both successful responses and errors
  processed_results <- results |>
    map(\(res) {
      if (inherits(res, "httr2_response") && resp_status(res) < 300) {
        # Success: Extract results from the response
        json_body <- tryCatch(
          resp_body_json(res),
          error = function(e) {
            warning("Failed to parse JSON response: ", conditionMessage(e))
            NULL
          }
        )

        # Check if parsing worked and 'translations' key exists
        if (is.null(json_body) || !("translations" %in% names(json_body))) {
          num_items_in_request <- length(res$request$body$data$text)
          warning("Failed to parse response or 'translations' key missing.")
          return(rep(NA_character_, num_items_in_request))
        }

        # Extract and process the text
        json_body[["translations"]] |>
          map(\(y) as_tibble(y)) |>
          list_rbind() |>
          mutate(
            text = if_else(.data$text == "NA", NA, .data$text),
            across(everything(), \(z) if_else(is.na(.data$text), NA, z))
          ) |>
          pull("text")
      } else if (inherits(res, "error")) {
        # Failure after retries: Generate NAs for this batch
        num_items_in_request <- 0
        # Try to get number of items from the request stored in the error
        if (!is.null(res$request) && !is.null(res$request$body$data$text)) {
          num_items_in_request <- length(res$request$body$data$text)
        } else {
          warning("Could not determine number of items for failed batch.")
        }
        warning(
          "Translation request failed for a batch: ",
          conditionMessage(res)
        )
        rep(NA_character_, num_items_in_request)
      } else {
        # Unexpected result type from req_perform_parallel
        warning(
          "Unexpected result type in parallel processing: ",
          class(res)[1]
        )
        # Cannot determine number of NAs easily, return NULL or empty vector
        character(0)
      }
    }) |>
    unlist() # Combine results from all batches

  # Ensure the output length matches the input length
  if (length(processed_results) != length(data)) {
    warning("Output length does not match input length. Check for errors.")
    # Attempt to resize, filling with NA
    length(processed_results) <- length(data)
  }

  # Restore NA values in the original positions
  processed_results[na_positions] <- NA_character_

  # Create a full-length vector for the final result
  final_results <- rep(NA_character_, length(data))
  # Place the processed results into the non-NA positions
  final_results[!na_positions] <- processed_results[!na_positions]

  # No need to restore NAs based on na_positions again,
  # as we started with a vector full of NAs.

  # Remove the old length check and NA restoration based on na_positions
  # Remove the conversion of data[na_positions] <- "NA" at the beginning

  return(final_results)
}
