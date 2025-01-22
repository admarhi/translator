#' Improve texts using the DeepL API
#'
#' @param data Character scalar or vector
#' @param target_lang Character scalar. Can only be any of x, x, x.
#' @param auth_key DeepL API key.
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_form req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom purrr map list_rbind
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else across
improve <- function(data, target_lang = NULL, auth_key) {
  resp <- request("https://api.deepl.com/v2/write/rephrase") |>
    req_headers(
      Authorization = paste0("DeepL-Auth-Key ", auth_key),
      Accept = "application/json"
    ) |>
    req_body_form(
      text = data,
      target_lang = target_lang,
      .multi = "explode"
    ) |>
    req_perform()

  resp_body_json(resp)$improvements |>
    map(\(x) as_tibble(x)) |>
    list_rbind() |>
    mutate(
      text = if_else(.data$text == "NA", NA, .data$text),
      across(2:3, \(x) if_else(is.na(.data$text), NA, x))
    ) |>
    dplyr::pull("text")
}
