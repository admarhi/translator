#' Split Strings into List
#'
#' Split strings into list based on a maximum kilobyte number. Helper function
#' to create requests.
#'
#' @param data Character vector
#' @param max_kib Numeric scalar giving the maximum size of vectors.
#'
#' @export
#'
#' @importFrom dplyr mutate group_by summarise pull lag
#' @importFrom tibble tibble
#'
#' @examples
#' split_into_list(c("hello", "world", "hallo", "welt"), max_kib = 0.1)
split_into_list <- function(data, max_kib = 100) {
  # Get number of groups required
  groups <- ceiling(get_size(data) / max_kib)

  tb <- tibble(
    string = data,
    kib = nchar(string, type = "bytes") / 1024,
    group = 0
  ) |>
    mutate(
      kib = if_else(is.na(.data$kib), 0, .data$kib),
      cum_kib = cumsum(.data$kib)
    )
  for (i in seq_len(groups)) {
    tb$cum_kib <- tb$cum_kib - max_kib
    tb$group[tb$cum_kib < 0 & tb$group == 0] <- i
  }
  tb |>
    group_by(group) |>
    summarise(strings_combined = list(string), .groups = "drop") |>
    pull(strings_combined)
}
