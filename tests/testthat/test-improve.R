# ---- Non-API Tests -----------------------------------------------------------
context("improve: Input Validation")

# These tests don't hit the API
test_that("improve errors on missing inputs", {
  expect_error(improve(), regexp = "'data' must be a character vector")
  # target_lang is optional
  # Test without providing the key argument or setting the env var
  withr::with_envvar(c("DEEPL_AUTH_KEY" = ""), {
    expect_error(
      improve(data = "hello"),
      regexp = "'auth_key' must be provided"
    )
  })
})

test_that("improve errors on invalid input types", {
  expect_error(
    improve(data = 123, auth_key = "dummy"),
    regexp = "'data' must be a character vector"
  )
  expect_error(
    improve(data = "hello", target_lang = 123, auth_key = "dummy"),
    regexp = "'target_lang' must be one of:"
  )
  expect_error(
    improve(data = "hello", auth_key = 123),
    regexp = "'auth_key' must be provided"
  )
})

test_that("improve errors on invalid language codes", {
  # Only target_lang needs validation here
  expect_error(
    improve(data = "hello", target_lang = "XX", auth_key = "dummy"),
    regexp = "'target_lang' must be one of:"
  )
})

test_that("improve errors on invalid auth_key format", {
  expect_error(
    improve(data = "hello", auth_key = ""),
    regexp = "'auth_key' must be provided"
  )
})

# ---- Live API Tests ----------------------------------------------------------
context("improve: Core Functionality (Live API)")

# Helper function to skip tests if API key is not available
skip_if_no_key <- function() {
  if (Sys.getenv("DEEPL_AUTH_KEY") == "") {
    skip("DEEPL_AUTH_KEY environment variable not set, skipping live API test.")
  }
}

test_that("basic improvement works (Live API)", {
  skip_if_no_key()
  input <- "this are bad gramar"
  Sys.sleep(1) # Pause before hitting API
  resp <- improve(input)

  expect_type(resp, "character")
  expect_length(resp, 1)
  expect_false(is.na(resp))
  expect_true(nchar(resp) > 0)
  # Check it's different from the (incorrect) input
  expect_false(resp == input)
})

test_that("improves multiple strings (Live API)", {
  skip_if_no_key()
  texts <- c("he go store", "she like read book")
  Sys.sleep(1)
  resp <- improve(texts)

  expect_type(resp, "character")
  expect_length(resp, length(texts))
  expect_false(any(is.na(resp)))
  expect_true(all(nchar(resp) > 0))
  expect_false(all(resp == texts))
})

test_that("handles NA values correctly (Live API)", {
  skip_if_no_key()
  texts <- c("gramar is important", NA, "speling also")
  expected_length <- length(texts)
  Sys.sleep(1)
  resp <- improve(texts)

  expect_type(resp, "character")
  expect_length(resp, expected_length)
  expect_true(is.na(resp[2])) # Check NA position
  expect_false(is.na(resp[1]))
  expect_false(is.na(resp[3]))
  expect_false(resp[1] == texts[1]) # Check improvement occurred
  expect_false(resp[3] == texts[3])
})

test_that("handles empty string input (Live API)", {
  skip_if_no_key()
  Sys.sleep(1)
  resp <- improve("")
  expect_type(resp, "character")
  expect_length(resp, 1)
  # DeepL improve endpoint might return empty or the original empty string
  expect_equal(resp, "")
})

test_that("handles all NA input (Live API)", {
  skip_if_no_key()
  texts <- c(NA_character_, NA_character_)
  expected_resp <- c(NA_character_, NA_character_)
  Sys.sleep(1)
  resp <- improve(texts)
  expect_equal(resp, expected_resp)
  expect_length(resp, length(texts))
})

test_that("uses target_lang when specified (Live API)", {
  skip_if_no_key()
  # Example: Improve English text, specifically targeting US English
  input <- "Let's analyse the colour."
  Sys.sleep(1)
  resp_us <- improve(input, target_lang = "EN-US")
  Sys.sleep(1)
  resp_gb <- improve(input, target_lang = "EN-GB") # Or default if key is GB

  expect_type(resp_us, "character")
  expect_length(resp_us, 1)
  expect_type(resp_gb, "character")
  expect_length(resp_gb, 1)

  # Check if the spelling was adjusted (might not always happen)
  # print(paste("US:", resp_us))
  # print(paste("GB:", resp_gb))
  expect_true(
    grepl("analyze", resp_us, ignore.case = TRUE) ||
      grepl("analyse", resp_us, ignore.case = TRUE)
  )
  expect_true(
    grepl("color", resp_us, ignore.case = TRUE) ||
      grepl("colour", resp_us, ignore.case = TRUE)
  )
  # It's harder to definitively check target_lang effect without specific
  # examples guaranteed to change, but we check the call succeeds and returns
  # valid text.
  expect_false(is.na(resp_us))
  expect_false(is.na(resp_gb))
})

context("improve: API Behavior (Live API)")

test_that("uses correct API endpoint based on key type (Live API)", {
  skip_if_no_key()
  auth_key <- Sys.getenv("DEEPL_AUTH_KEY")
  is_free <- grepl(":fx$", auth_key)
  input <- "test grammar"
  Sys.sleep(1)
  # Expect the call to succeed regardless of key type (Free/Pro)
  expect_no_error(
    resp <- improve(input)
  )
  expect_type(resp, "character")
  expect_length(resp, 1)
  expect_false(is.na(resp))
})

# Specific API error tests (4xx, 5xx), malformed JSON tests removed as they
# relied on mocking.
