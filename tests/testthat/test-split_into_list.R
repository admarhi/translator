# Helper to create repeatable large-ish strings
create_string <- function(n_chars) {
  paste(rep(letters, length.out = n_chars), collapse = "")
}

# Note: 1 char (ASCII) = 1 byte. 1 KiB = 1024 bytes.
# String of 1024 chars is approx 1 KiB.

test_that("splits basic vector correctly", {
  s1k <- create_string(1024) # ~1 KiB
  s500 <- create_string(500) # ~0.5 KiB
  input <- c(s1k, s500, s1k, s500, s1k) # Total ~4 KiB

  max_k <- 1.6 # Target max size
  result <- split_into_list(input, max_kib = max_k)

  # Expected batches:
  # 1: s1k, s500 (~1.5 KiB <= 1.6) -> OK
  # 2: s1k, s500 (~1.5 KiB <= 1.6) -> OK
  # 3: s1k (~1 KiB <= 1.6) -> OK
  expect_type(result, "list")
  expect_length(result, 3)
  expect_equal(unlist(result), input) # Check content and order
})

test_that("handles case where all fit in one batch", {
  input <- c("hello", "world")
  max_k <- 1 # More than enough
  result <- split_into_list(input, max_kib = max_k)

  expect_length(result, 1)
  expect_equal(result[[1]], input)
})

test_that("empty input returns empty list", {
  result <- split_into_list(character(0), max_kib = 1)
  expect_equal(result, list())
})

test_that("input with empty strings works", {
  input <- c("a", "", "b", "", "", "c")
  max_k <- 0.001 # ~1 byte
  result <- split_into_list(input, max_kib = max_k)

  # Expected: [["a", ""], ["b", "", ""], ["c"]]
  expect_length(result, 3)
  expect_equal(unlist(result), input)
  expect_equal(result[[1]], c("a", ""))
  expect_equal(result[[2]], c("b", "", ""))
  expect_equal(result[[3]], "c")
})

test_that("batch sizes are mostly below max_kib", {
  s1k <- create_string(1024) # ~1 KiB
  s500 <- create_string(500) # ~0.5 KiB
  input <- c(s1k, s500, s1k, s500, s1k) # Total ~4 KiB
  max_k <- 1.6

  result <- split_into_list(input, max_kib = max_k)
  batch_sizes <- vapply(result, get_size, numeric(1))

  # Check that *all* batches are <= max_kib in this case
  expect_true(all(batch_sizes <= max_k))
})

test_that("handles single item exceeding max_kib", {
  s2k <- create_string(2048) # ~2 KiB
  s500 <- create_string(500) # ~0.5 KiB
  input <- c(s500, s2k, s500) # Total ~3 KiB
  max_k <- 1.0 # Max 1 KiB

  result <- split_into_list(input, max_kib = max_k)
  batch_sizes <- vapply(result, get_size, numeric(1))

  # Expected batches:
  # 1: s500 (~0.5 KiB <= 1.0) -> OK
  # 2: s2k (~2.0 KiB > 1.0) -> Expected violation
  # 3: s500 (~0.5 KiB <= 1.0) -> OK
  expect_length(result, 3)
  expect_equal(result[[1]], input[1])
  expect_equal(result[[2]], input[2]) # s2k is alone
  expect_equal(result[[3]], input[3])

  # Check sizes
  expect_lte(batch_sizes[1], max_k)
  expect_gt(batch_sizes[2], max_k) # Verify the violation for the large item
  expect_lte(batch_sizes[3], max_k)
})

test_that("size constraint holds with many small items", {
  s10 <- create_string(10) # ~0.01 KiB
  input <- rep(s10, 200) # ~2 KiB total
  max_k <- 0.5 # ~50 items per batch

  result <- split_into_list(input, max_kib = max_k)
  batch_sizes <- vapply(result, get_size, numeric(1))

  # Expect 4 batches (200 * 0.01 / 0.5 = 4)
  expect_length(result, 4)
  # All batches should be under the limit
  expect_true(all(batch_sizes <= max_k))
  # Check total items preserved
  expect_equal(length(unlist(result)), length(input))
})

test_that("errors on non-character input", {
  expect_error(split_into_list(1:10), regexp = "must be a character vector")
  expect_error(
    split_into_list(list("a", "b")),
    regexp = "must be a character vector"
  )
})
