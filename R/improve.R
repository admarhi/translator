#' Improve texts using the DeepL API
#'
#' This function sends text to DeepL's writing improvement API and returns the
#' enhanced results. It handles batching of large text inputs and parallel
#' processing of improvement requests.
#'
#' @param data Character scalar or vector to be improved. Required.
#' @param target_lang Chr scalar specifying the target language (optional).
#'        If NULL (default), the original text language will be used.
#'        Must be one of the supported DeepL language codes (see Details).
#' @param auth_key Character scalar containing the DeepL API authentication key.
#'        Required. Can be provided directly or set as environment variable
#'        'DEEPL_AUTH_KEY'.
#' @param max_request_size Numeric scalar specifying the maximum request size in
#'        kilobytes (default: 5). DeepL limits individual requests for the
#'        improvement endpoint.
#' @param timeout Numeric scalar specifying the timeout for API requests in
#'        seconds (default: 30).
#'
#' @details
#' Supported language codes are:
#' "DE" (German),
#' "EN-GB" (British English),
#' "EN-US" (American English),
#' "ES" (Spanish),
#' "FR" (French),
#' "IT" (Italian),
#' "PT-BR" (Brazilian Portuguese),
#' "PT-PT" (European Portuguese)
#'
#' @return Character vector of improved text. Will preserve the length and
#'         order of the input. NA values in the input will remain NA in the
#'         output.
#'
#' @examples
#' \dontrun{
#' # Simple text improvement
#' improve("Hello world", auth_key = "your-api-key")
#'
#' # Improve multiple strings with specified target language
#' texts <- c("Hello", "How are you?", "Thank you")
#' improve(texts,
#'   target_lang = "EN-US",
#'   auth_key = "your-api-key"
#' )
#'
#' # Using an environment variable for authentication
#' Sys.setenv(DEEPL_AUTH_KEY = "your-api-key")
#' improve("Hello world")
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_form req_perform_parallel
#' @importFrom httr2 req_timeout req_retry resp_status resp_body_json
#' @importFrom purrr map list_rbind
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else across pull everything
improve <- function(
  data,
  target_lang = NULL,
  auth_key = Sys.getenv("DEEPL_AUTH_KEY"),
  max_request_size = 5,
  timeout = 30
) {
  # Input validation
  if (missing(data) || !is.character(data)) {
    stop("'data' must be a character vector")
  }

  # Vector of valid improve languages
  VALID_IMPROVE_LANGUAGES <- c(
    "DE",
    "EN-GB",
    "EN-US",
    "ES",
    "FR",
    "IT",
    "PT-BR",
    "PT-PT"
  )
  # Validate target_lang if provided
  if (!is.null(target_lang) && !target_lang %in% VALID_IMPROVE_LANGUAGES) {
    stop(
      "'target_lang' must be one of: ",
      paste(VALID_IMPROVE_LANGUAGES, collapse = ", ")
    )
  }

  # Validate auth_key
  if (auth_key == "" || !is.character(auth_key) || length(auth_key) != 1) {
    stop(
      "'auth_key' must be provided either directly or via the ",
      "DEEPL_AUTH_KEY environment variable"
    )
  }

  # Determine API endpoint based on the key (free vs pro)
  api_url <- if (grepl("^[a-zA-Z0-9]+:fx", auth_key)) {
    "https://api-free.deepl.com/v2/write/rephrase"
  } else {
    "https://api.deepl.com/v2/write/rephrase"
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
    req_timeout(timeout) |>
    # Retry on transient errors (5xx, 429) up to 3 times with backoff
    req_retry(
      max_tries = 3,
      is_transient = ~ resp_status(.x) %in% c(429, 500, 502, 503, 504)
    )

  # Add target_lang if specified
  if (!is.null(target_lang)) {
    req_base <- req_body_form(req_base, target_lang = target_lang)
  }

  # Prepare batches for parallel processing
  batches <- data |>
    split_into_list(max_kib = max_request_size) |>
    map(\(x) req_body_form(req_base, text = x, .multi = "explode"))

  # Execute improvement requests in parallel batches
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

        # Check if parsing worked and 'improvements' key exists
        if (is.null(json_body) || !("improvements" %in% names(json_body))) {
          num_items_in_request <- length(res$request$body$data$text)
          warning("Failed to parse response or 'improvements' key missing.")
          return(rep(NA_character_, num_items_in_request))
        }

        # Extract and process the text (logic adapted from extract_resp_deepl)
        json_body[["improvements"]] |>
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
          "Improvement request failed for a batch: ",
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
  # This is a safeguard, especially if errors occurred determining batch sizes
  if (length(processed_results) != length(data)) {
    warning("Output length does not match input length. Check for errors.")
    # Attempt to resize, filling with NA, though this might be incorrect
    length(processed_results) <- length(data)
  }

  # Restore NA values in the original positions
  processed_results[na_positions] <- NA_character_

  # Return the improvements
  processed_results
}
