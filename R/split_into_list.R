#' Split Strings into List
#' 
#' Split strings into list based on a maximum kilobyte number. Helper function 
#' to create requests.
#' 
#' @export
#' 
#' @importFrom dplyr mutate group_by summarise pull
#' @importFrom tibble tibble
#' 
#' @examples
#' split_into_list(c("hello", "world", "hallo", "welt"), max_kib = 0.1)
split_into_list <- function(strings, max_kib = 100) {
  tibble(
    string = strings,
    kib = nchar(strings, type = "bytes") / 1024
  ) |>
    mutate(
      kib = if_else(is.na(.data$kib), 0, .data$kib),
      cum_kib = cumsum(kib),
      group = cumsum(lag(cumsum(kib), default = 0) %% max_kib + kib > max_kib)
    ) |>
    group_by(group) |>
    summarise(strings_combined = list(string), .groups = "drop") |>
    pull(strings_combined)
}
