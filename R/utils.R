#' @title Get Character Vector Size in kb
#'
#' @param data Character vector
#' @return Numeric scalar giving the size of the character vector in kilobytes.
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

#' @title Extract Response Body from DeepL
#'
#' @description
#' Function to extract translations or improvements from a DeepL API response.
#'
#' @param data A list of httr2 responses
#' @param name Character scalar giving the key under which the text is stored.
#' @return Character vector of translated/improved text.
#'
#' @export
#'
#' @importFrom purrr map list_rbind
#' @importFrom httr2 resp_body_json
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate if_else across pull everything
extract_resp_deepl <- function(data, name) {
  data |>
    map(\(x) resp_body_json(x)[[name]] |>
      map(\(y) as_tibble(y)) |>
      list_rbind() |>
      pull("text")) |>
    unlist()
}

#' @title DeepL API Valid Source Language Codes
#'
#' @description
#' Vector containing all valid source language codes supported by the DeepL API
#' translation endpoint.
#'
#' @format A character vector
#' @keywords internal
#' @seealso [VALID_TRANSLATE_TARGET_LANGUAGES]
VALID_TRANSLATE_SOURCE_LANGUAGES <- c(
  "BG", "CS", "DA", "DE", "EL", "EN", "ES", "ET", "FI", "FR", "HU", "ID", "IT", 
  "JA", "KO", "LT", "LV", "NB", "NL", "PL", "PT", "RO", "RU", "SK", "SL", "SV", 
  "TR", "UK", "ZH"
)

#' @title DeepL API Valid Target Language Codes
#'
#' @description
#' Vector containing all valid target language codes supported by the DeepL API
#' translation endpoint.
#'
#' @format A character vector
#' @keywords internal
#' @seealso [VALID_TRANSLATE_SOURCE_LANGUAGES]
VALID_TRANSLATE_TARGET_LANGUAGES <- c(
  "AR", "BG", "CS", "DA", "DE", "EL", "EN-GB", "EN-US", "ES", "ET", "FI", "FR", 
  "HU", "ID", "IT", "JA", "KO", "LT", "LV", "NB", "NL", "PL", "PT-BR", "PT-PT",
  "RO", "RU", "SK", "SL", "SV", "TR", "UK", "ZH"
)


#' @title DeepL API Valid Improve Language Codes
#'
#' @description
#' Vector containing all valid language codes supported by the DeepL API
#' improve endpoint.
#'
#' @format A character vector
#' @keywords internal
VALID_IMPROVE_LANGUAGES <- c(
  "DE", "EN", "EN-GB", "EN-US", "ES", "FR", "IT", "PT-BR", "PT-PT"
)