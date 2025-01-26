#' Translate Texts Using the DeepL API
#'
#' @param data Character scalar or vector
#' @param target_lang Character scalar. Can only be any of x, x, x.
#' @param auth_key DeepL API key.
#' @export
#'
#' @importFrom httr2 request req_headers req_body_form req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom purrr map list_rbind
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else across
translate <- function(
    data,
    target_lang,
    source_lang = NULL,
    auth_key) {
  ### Validate that data is chr vctr

  # convert NA to "NA" to allow for accurate size computation
  data[is.na(data)] <- "NA"

  # Set up the key
  key <- paste0("DeepL-Auth-Key ", auth_key)

  # Construct base request
  req_base <- 
    request("https://api.deepl.com/v2/translate") |> 
    req_headers(Authorization = key, Accept = "application/json") |> 
    req_body_form(target_lang = target_lang) |> 
    req_timeout(20)

  data |> 
    split_into_list(max_kib = 80) |> 
    map(\(x) req_body_form(req_base, text = x, .multi = "explode")) |> 
    req_perform_parallel(
      progress = "Translating Text", on_error = "continue"
    ) |> 
    extract_resp_deepl(name = "translations")


}
