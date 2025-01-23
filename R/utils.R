#' Get Character Vector Size in kb
#'
#' @param data Character vector
#'
#' @export
#'
#' @examples
#' get_size("hello world")
#' get_size(c("hello world", "Hallo Welt"))
get_size <- function(data) {
  bytes <- nchar(data, type = "bytes") |> sum(na.rm = TRUE)
  bytes / 1024
}

#' Extract Response Body from DeepL
#' 
#' @param data A list of httr2 responses
#' @param name Character scalar giving the key under which the text is stored.
#' 
#' @export
#' 
#' @importFrom purrr map list_rbind
#' @importFrom httr2 resp_body_json
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else across pull
#' 
extract_resp_deepl <- function(data, name) {
  data |> 
    map(\(x) resp_body_json(x)[[name]] |>
      map(\(y) as_tibble(y)) |>
      list_rbind() |>
      mutate(
        text = if_else(.data$text == "NA", NA, .data$text),
        across(2:3, \(x) if_else(is.na(.data$text), NA, x))
      ) |>
      pull("text")) |> 
      unlist()
}