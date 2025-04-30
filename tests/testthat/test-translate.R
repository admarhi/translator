# ---- Non-API Tests -----------------------------------------------------------

# These tests don't hit the API, so they don't need skipping
test_that("translate errors on missing inputs", {
  expect_error(
    translate(target_lang = "DE"),
    regexp = "'data' must be a character vector"
  )
  expect_error(
    translate(data = "hello"),
    regexp = "'target_lang' must be one of:"
  )
  # Test without providing the key argument or setting the env var
  withr::with_envvar(c("DEEPL_AUTH_KEY" = ""), {
    expect_error(
      translate(data = "hello", target_lang = "DE"),
      regexp = "'auth_key' must be provided"
    )
  })
})

test_that("translate errors on invalid input types", {
  expect_error(
    translate(data = 123, target_lang = "DE", auth_key = "dummy"),
    regexp = "'data' must be a character vector"
  )
  expect_error(
    translate(data = "hello", target_lang = 123, auth_key = "dummy"),
    regexp = "'target_lang' must be one of:"
  )
  expect_error(
    translate(data = "hello", target_lang = "DE", auth_key = 123),
    regexp = "'auth_key' must be provided"
  )
})

test_that("translate errors on invalid language codes", {
  expect_error(
    translate(data = "hello", target_lang = "XX", auth_key = "dummy"),
    regexp = "'target_lang' must be one of:"
  )
  expect_error(
    translate(
      data = "hello",
      target_lang = "DE",
      source_lang = "YY",
      auth_key = "dummy"
    ),
    regexp = "'source_lang' must be one of:"
  )
})

test_that("translate errors on invalid formality", {
  expect_error(
    translate(
      data = "hello",
      target_lang = "DE",
      formality = "invalid",
      auth_key = "dummy"
    ),
    regexp = "'formality' must be one of:"
  )
})

test_that("translate errors on invalid auth_key format", {
  expect_error(
    translate(data = "hello", target_lang = "DE", auth_key = ""),
    regexp = "'auth_key' must be provided"
  )
})

# ---- Live API Tests ----------------------------------------------------------

# Helper function to skip tests if API key is not available
skip_if_no_key <- function() {
  if (Sys.getenv("DEEPL_AUTH_KEY") == "") {
    skip("DEEPL_AUTH_KEY environment variable not set, skipping live API test.")
  }
}

test_that("basic translation works (Live API)", {
  skip_if_no_key()
  input <- "hello"
  Sys.sleep(1) # Pause before hitting API
  resp <- translate(input, target_lang = "DE")

  expect_type(resp, "character")
  expect_length(resp, 1)
  expect_false(is.na(resp))
  expect_true(nchar(resp) > 0)
  # Optional: check it's likely German (e.g., not identical to input)
  expect_false(resp == input)
})

test_that("translates multiple strings (Live API)", {
  skip_if_no_key()
  texts <- c("hello", "world")
  Sys.sleep(1)
  resp <- translate(texts, target_lang = "ES")

  expect_type(resp, "character")
  expect_length(resp, length(texts))
  expect_false(any(is.na(resp)))
  expect_true(all(nchar(resp) > 0))
  expect_false(all(resp == texts))
})

test_that("handles NA values correctly (Live API)", {
  skip_if_no_key()
  texts <- c("hello", NA, "world")
  expected_length <- length(texts)
  Sys.sleep(1)
  resp <- translate(texts, target_lang = "FR")

  expect_type(resp, "character")
  expect_length(resp, expected_length)
  expect_true(is.na(resp[2])) # Check NA position
  expect_false(is.na(resp[1]))
  expect_false(is.na(resp[3]))
  expect_false(resp[1] == texts[1]) # Check translation occurred
  expect_false(resp[3] == texts[3])
})

test_that("handles empty string input (Live API)", {
  skip_if_no_key()
  Sys.sleep(1)
  resp <- translate("", target_lang = "DE")
  expect_type(resp, "character")
  expect_length(resp, 1)
  # DeepL typically returns an empty string for an empty input
  expect_equal(resp, "")
})

test_that("handles all NA input (Live API)", {
  # This shouldn't hit the API if pre-checked, but confirm behavior
  # The function currently converts NA->"NA"->NA, so it might still hit API.
  # Let's keep the skip guard.
  skip_if_no_key()
  texts <- c(NA_character_, NA_character_)
  expected_resp <- c(NA_character_, NA_character_)
  Sys.sleep(1)
  resp <- translate(texts, target_lang = "DE")
  expect_equal(resp, expected_resp)
  expect_length(resp, length(texts))
})

test_that("uses source_lang when specified (Live API)", {
  skip_if_no_key()
  # Example: Translate German "Haus" to English, specifying source
  input <- "Haus"
  Sys.sleep(1)
  resp <- translate(input, target_lang = "EN-US", source_lang = "DE")
  expect_type(resp, "character")
  expect_length(resp, 1)
  # Basic check for English translation
  expect_true(tolower(resp) %in% c("house", "building")) # Allow variations
})

test_that("uses formality when specified (Live API)", {
  skip_if_no_key()
  # Example: German formal vs informal 'you'
  input <- "Wie geht es Ihnen?"
  Sys.sleep(1)
  resp_formal <- translate(
    input,
    target_lang = "EN-US",
    formality = "prefer_more"
  )
  Sys.sleep(1)
  resp_informal <- translate(
    input,
    target_lang = "EN-US",
    formality = "prefer_less"
  )

  expect_type(resp_formal, "character")
  expect_length(resp_formal, 1)
  expect_type(resp_informal, "character")
  expect_length(resp_informal, 1)
  # It's hard to guarantee different outputs, but check they're valid
  expect_true(nchar(resp_formal) > 0)
  expect_true(nchar(resp_informal) > 0)
  # May or may not differ for this specific phrase in English
  # print(paste("Formal:", resp_formal))
  # print(paste("Informal:", resp_informal))
})

# Note: Testing specific API errors like 403 (bad key) or 456 (quota)
# is difficult in automated tests without intentionally using invalid keys
# or exceeding quotas. Input validation tests cover errors before API calls.
# Transient errors (5xx) are handled by httr2's retry, which is hard to test directly.
# We focus on the function's behavior with valid inputs/key.

test_that("uses correct API endpoint based on key type (Live API)", {
  skip_if_no_key()
  auth_key <- Sys.getenv("DEEPL_AUTH_KEY")
  is_free <- grepl(":fx$", auth_key)
  input <- "test"
  Sys.sleep(1)
  # We can't intercept the URL, but we expect the call to succeed
  # regardless of key type, assuming the key is valid.
  expect_no_error(
    resp <- translate(input, target_lang = "DE")
  )
  expect_type(resp, "character")
  expect_length(resp, 1)
  expect_false(is.na(resp))
})

# No need to test malformed JSON / missing keys from API side now
# No need to test output length matching input with errors in the same way
