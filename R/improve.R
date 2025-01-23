#' Improve texts using the DeepL API
#'
#' @param data Character scalar or vector
#' @param target_lang Character scalar. Can only be any of x, x, x.
#' @param auth_key DeepL API key.
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_form req_perform_parallel
#' @importFrom httr2 req_timeout
#' @importFrom purrr map
improve <- function(data, target_lang = NULL, auth_key) {
  ### Validate that data is chr vctr

  # convert NA to "NA" to allow for accurate size counting
  data[is.na(data)] <- "NA"

  # Set up key
  key <- paste0("DeepL-Auth-Key ", auth_key)

  # Set up the base request
  req_base <- 
    request("https://api.deepl.com/v2/write/rephrase") |>
    req_headers(Authorization = key, Accept = "application/json") |>
    req_body_form(target_lang = target_lang) |> 
    req_timeout(20)

  data |> 
    split_into_list(max_kib = 5) |> 
    map(\(x) req_body_form(req_base, text = x, .multi = "explode")) |>
    req_perform_parallel(progress = "Improving Text", on_error = "continue") |> 
    extract_resp_deepl(name = "improvements")
}
