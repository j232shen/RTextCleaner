library(testthat)
library(httr)

test_input_1 <- c("The quick brown fox jumps over the lazy dog.")
test_input_2 <- rep(test_input_1, times = 2)


test_that("summarize returns a character vector with correct length", {
  with_mock(
    `httr::POST` = function(...) {
      # return a mock response object with success status code
      structure(list(status_code = 200), class = "response")
    },
    `httr::content` = function(response) {
      # return mock content structure extracted from response
      list(
        candidates = list(
          list(
            content = list(
              parts = list(list(text = "Summary text"))
            )
          )
        )
      )
    },
    {
      result <- summarize(test_input_2)
      expect_length(result, 2) # check result length
      expect_type(result, "character") # check result type
      expect_equal(result, c("Summary text", "Summary text")) # ensure that it matches the expected summary text
    }
  )
})


test_that("summarize handles different responses for multiple inputs", {
  # mock to return different responses for each call
  call_count <- 0

  with_mock(
    `httr::POST` = function(...) {
      structure(list(status_code = 200), class = "response")
    },
    `httr::content` = function(response) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(candidates = list(list(content = list(parts = list(list(text = "Summary 1")))))))
      } else {
        return(list(candidates = list(list(content = list(parts = list(list(text = "Summary 2")))))))
      }
    },
    {
      result <- summarize(c("Text 1", "Text 2"))
      expect_equal(result, c("Summary 1", "Summary 2"))
    }
  )
})


test_that("summarize prompts for API key when none is available", {
  original_api_key <- Sys.getenv("GEMINI_API_KEY") # save current API key
  Sys.unsetenv("GEMINI_API_KEY")  # clear the API key
  on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key)) # restore API key after test

  with_mock(
    `readline` = function(prompt) {
      expect_match(prompt, "Paste your API key here")
      return("fake_test_api_key")
    },
    `httr::POST` = function(...) {
      structure(list(status_code = 200), class = "response")
    },
    `httr::content` = function(response) {
      list(candidates = list(list(content = list(parts = list(list(text = "Summary"))))))
    },
    {
      result <- summarize(c("Test text"))
      expect_equal(result, "Summary") # check that the function returns a result after asking for the key
    }
  )
})


test_that("summarize handles invalid API key error", {
  original_api_key <- Sys.getenv("GEMINI_API_KEY") # save current API key
  on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key)) # restore API key after test

  correct_var_unset <- FALSE # flag to track if the right environment variable was unset

  with_mock(
    `httr::POST` = function(...) {
      # return a mock error response with 400 status code
      structure(list(status_code = 400), class = "response")
    },
    `httr::content` = function(response) {
      # return error message extracted from response
      list(
        error = list(
          message = "API key not valid. Please pass a valid API key."
        )
      )
    },
    `Sys.unsetenv` = function(x) { # mock unsets all environment variables
      if (any(x == "GEMINI_API_KEY")) { # check if any of the variables is the one we're looking for
        correct_var_unset <<- TRUE
      }
    },
    {
      # check for correct error
      expect_error(
        summarize(test_input_1),
        "Error: API key not valid. Please pass a valid API key."
      )
      # verify that GEMINI_API_KEY was unset
      expect_true(correct_var_unset, "GEMINI_API_KEY was not unset")
    }
  )
})


test_that("summarize throws an error on API failure", {
  with_mock(
    `httr::POST` = function(...) {
      # return a mock error response with 400 status code
      structure(list(status_code = 400), class = "response")
    },
    `httr::content` = function(response) {
      # return error message extracted from response
      list(error = list(message = "Invalid request"))
    },
    {
      expect_error(summarize(test_input_1), "Error: Invalid request")
    }
  )
})


test_that("summarize waits when rate limit is reached", {
  request_times <<- rep(Sys.time(), 15)

  with_mock(
    `httr::POST` = function(...) {
      # return a mock response object with success status code
      structure(list(status_code = 200), class = "response")
    },
    `httr::content` = function(response) {
      # return mock content structure extracted from response
      list(
        candidates = list(
          list(
            content = list(
              parts = list(list(text = "Rate-limited response"))
            )
          )
        )
      )
    },
    `Sys.sleep` = function(x) {
      expect_gt(x, 0) # ensure it actually waits
    },
    {
      result <- summarize(test_input_1)
      expect_type(result, "character")
      expect_equal(result, rep("Rate-limited response", length(test_input_1)))
    }
  )
})


test_that("summarize throws an error for empty input", {
  expect_error(summarize(character(0)), "Error: text_inputs cannot be empty")
})


test_that("summarize throws an error for non-character input", {
  expect_error(summarize(42), "Error: text_inputs must be a character vector.")
  expect_error(summarize(list("valid", 123)), "Error: text_inputs must be a character vector.")
  expect_error(summarize(TRUE), "Error: text_inputs must be a character vector.")
})
