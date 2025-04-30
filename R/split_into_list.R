#' Split Character Vector into List by Size
#'
#' Splits a character vector into a list of character vectors, where each
#' sub-vector's total size in kilobytes (calculated using `nchar(type='bytes')`)
#' is approximately at most `max_kib`. This is a helper function primarily used
#' to prepare data batches for API requests with size limits.
#'
#' @details
#' The function uses a greedy approach. It iterates through the input vector,
#' adding elements to the current batch until adding the next element would
#' exceed `max_kib`. At that point, the current batch is finalized, and a new
#' batch starts with the element that didn't fit.
#'
#' *Edge Case:* If a single string by itself exceeds `max_kib`, it will be
#' placed in its own sub-vector in the list, which will necessarily exceed the
#' limit.
#'
#' @param data Character vector to be split.
#' @param max_kib Numeric scalar. The target maximum size in kilobytes for each
#'   sub-vector in the output list. Defaults to 100 KiB.
#'
#' @return A list, where each element is a character vector representing a batch
#'
#' @export
#'
#' @examples
#' # approx 10.7 KiB
#' long_text <- paste(rep("abcde fghij", 1000), collapse = " ")
#' short_text <- "short"
#' items <- c(long_text, short_text, long_text, short_text)
#'
#' # Split into batches of max ~15 KiB
#' batches_15kib <- split_into_list(items, max_kib = 15)
#' length(batches_15kib) # Should be 2 batches
#'
#' # Verify sizes (approximate)
#' get_size(batches_15kib[[1]]) # Should be ~10.7 KiB (long_text + short_text)
#' get_size(batches_15kib[[2]]) # Should be ~10.7 KiB (long_text + short_text)
#'
#' # Split into batches of max ~10 KiB (forces long_text into its own batch)
#' batches_10kib <- split_into_list(items, max_kib = 10)
#' length(batches_10kib) # Should be 4 batches
#' get_size(batches_10kib[[1]]) # ~10.7 KiB (violates limit due to single item)
#' get_size(batches_10kib[[2]]) # Very small (short_text)
#'
#' # Empty input
#' split_into_list(character(0))
#'
#' # All fit in one batch
#' split_into_list(c("hello", "world"), max_kib = 1)
split_into_list <- function(data, max_kib = 100) {
  if (!is.character(data)) {
    stop("'data' must be a character vector")
  }
  if (length(data) == 0) {
    return(list())
  }

  string_kibs <- nchar(data, type = "bytes", allowNA = FALSE) / 1024
  # Handle potential NAs from nchar if invalid encoding occurs, treat as 0 size
  string_kibs[is.na(string_kibs)] <- 0

  batches <- list()
  current_batch <- character(0)
  current_kib <- 0

  for (i in seq_along(data)) {
    item_kib <- string_kibs[i]
    item_string <- data[i]

    # If the current batch is empty, add the item regardless of its size
    # (handles single items larger than max_kib)
    if (length(current_batch) == 0) {
      current_batch <- item_string
      current_kib <- item_kib
    } else if (current_kib + item_kib <= max_kib) {
      # Add item to current batch if it fits
      current_batch <- c(current_batch, item_string)
      current_kib <- current_kib + item_kib
    } else {
      # Finalize the current batch (it's full)
      batches[[length(batches) + 1]] <- current_batch
      # Start a new batch with the current item
      current_batch <- item_string
      current_kib <- item_kib
    }
  }

  # Add the last batch if it contains items
  if (length(current_batch) > 0) {
    batches[[length(batches) + 1]] <- current_batch
  }

  return(batches)
}
