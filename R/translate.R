#' Translate Texts Using the DeepL API
#'
#' This function sends text to DeepL's translation API and returns the
#' translated results. It handles batching of large text inputs and parallel
#' processing of translation requests.
#'
#' @param data Character scalar or vector to be translated. Required.
#' @param target_lang Character scalar specifying the target language. Must be
#' one of the supported DeepL language codes (see Details).
#' @param source_lang Character scalar specifying the source language
#' (optional). If NULL (default), DeepL will auto-detect the source language.
#' Must be one of the supported DeepL language codes (see Details).
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
#' @details
#' Supported language codes for both source and target languages are:
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
#' @importFrom httr2 request req_headers req_body_form req_perform
#' @importFrom httr2 resp_body_json req_perform_parallel req_timeout
#' @importFrom purrr map list_rbind
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else across
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
  if (missing(target_lang) || !target_lang %in% valid_langs) {
    stop(
      "'target_lang' must be one of: ",
      paste(valid_langs, collapse = ", ")
    )
  }

  # Validate source_lang if provided
  if (!is.null(source_lang) && !source_lang %in% valid_langs) {
    stop(
      "'source_lang' must be one of: ",
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

  # Construct base request
  req_base <-
    request(api_url) |>
    req_headers(Authorization = key, Accept = "application/json") |>
    req_body_form(target_lang = target_lang) |>
    req_timeout(timeout)

  # Add source_lang if specified
  if (!is.null(source_lang)) {
    req_base <- req_body_form(req_base, source_lang = source_lang)
  }

  # Add formality if not default
  if (formality != "default") {
    req_base <- req_body_form(req_base, formality = formality)
  }

  # Function to handle request errors
  handle_errors <- function(resp) {
    if (inherits(resp, "error")) {
      warning("Translation request failed: ", conditionMessage(resp))
      return(rep(NA_character_, length(resp$request$body$text)))
    }
    return(resp)
  }

  # Execute translation requests in parallel batches
  translations <- tryCatch(
    {
      data |>
        split_into_list(max_kib = max_request_size) |>
        map(\(x) req_body_form(req_base, text = x, .multi = "explode")) |>
        req_perform_parallel(
          progress = TRUE,
          on_error = "continue"
        ) |>
        map(handle_errors) |>
        extract_resp_deepl(name = "translations")
    },
    error = function(e) {
      stop("Translation failed: ", conditionMessage(e))
    }
  )

  # Restore NA values in the original positions
  translations[na_positions] <- NA_character_

  # Return the translations
  translations
}
