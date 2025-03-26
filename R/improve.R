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
#' "BG" (Bulgarian), "CS" (Czech), "DA" (Danish), "DE" (German),
#' "EL" (Greek), "EN" (English), "EN-GB" (British English),
#' "EN-US" (American English), "ES" (Spanish), "ET" (Estonian),
#' "FI" (Finnish), "FR" (French), "HU" (Hungarian), "ID" (Indonesian),
#' "IT" (Italian), "JA" (Japanese), "KO" (Korean), "LT" (Lithuanian),
#' "LV" (Latvian), "NB" (Norwegian), "NL" (Dutch), "PL" (Polish),
#' "PT" (Portuguese), "PT-BR" (Brazilian Portuguese),
#' "PT-PT" (European Portuguese), "RO" (Romanian), "RU" (Russian),
#' "SK" (Slovak), "SL" (Slovenian), "SV" (Swedish), "TR" (Turkish),
#' "UK" (Ukrainian), "ZH" (Chinese).
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
#' @importFrom httr2 req_timeout
#' @importFrom purrr map
improve <- function(
    data,
    target_lang = NULL,
    auth_key = Sys.getenv("DEEPL_AUTH_KEY"),
    max_request_size = 5,
    timeout = 30) {
  # Input validation
  if (missing(data) || !is.character(data)) {
    stop("'data' must be a character vector")
  }

  # Validate target_lang if provided
  if (!is.null(target_lang) && !target_lang %in% valid_langs) {
    stop(
      "'target_lang' must be one of: ",
      paste(valid_langs, collapse = ", ")
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

  # Construct base request
  req_base <-
    request(api_url) |>
    req_headers(Authorization = key, Accept = "application/json") |>
    req_timeout(timeout)

  # Add target_lang if specified
  if (!is.null(target_lang)) {
    req_base <- req_body_form(req_base, target_lang = target_lang)
  }

  # Function to handle request errors
  handle_errors <- function(resp) {
    if (inherits(resp, "error")) {
      warning("Improvement request failed: ", conditionMessage(resp))
      return(rep(NA_character_, length(resp$request$body$text)))
    }
    return(resp)
  }

  # Execute improvement requests in parallel batches
  improvements <- tryCatch(
    {
      data |>
        split_into_list(max_kib = max_request_size) |>
        map(\(x) req_body_form(req_base, text = x, .multi = "explode")) |>
        req_perform_parallel(
          progress = TRUE,
          on_error = "continue"
        ) |>
        map(handle_errors) |>
        extract_resp_deepl(name = "improvements")
    },
    error = function(e) {
      stop("Text improvement failed: ", conditionMessage(e))
    }
  )

  # Restore NA values in the original positions
  improvements[na_positions] <- NA_character_

  # Return the improvements
  improvements
}
