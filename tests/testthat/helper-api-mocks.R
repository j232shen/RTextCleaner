# Helper Functions for API Tests

#' Mock API Success Response
#'
#' Creates a list of mock functions for simulating successful API responses
#'
#' @param response_text Text to return in the mock response
#' @return List of mock functions for use with with_mock()
mock_api_success <- function(response_text = "Success response") {
  list(
    `httr::POST` = function(...) {
      structure(list(status_code = 200), class = "response")
    },
    `httr::content` = function(response) {
      list(
        candidates = list(
          list(
            content = list(
              parts = list(list(text = response_text))
            )
          )
        )
      )
    }
  )
}

#' Test Function with API Success Mock
#'
#' Tests a function with a mocked successful API response
#'
#' @param func Function to test
#' @param input Input to pass to the function
#' @param response_text Text to return in the mock response
#' @return Result of the function call
test_with_api_success <- function(func, input, response_text = "Success response") {
  mocks <- mock_api_success(response_text)
  with_mock(
    .env = mocks,
    {
      result <- func(input)
      expect_type(result, "character")
      if (length(input) > 1) {
        expect_equal(result, rep(response_text, length(input)))
      } else {
        expect_equal(result, response_text)
      }
      return(result)
    }
  )
}

#' Mock API Error Response
#'
#' Creates a list of mock functions for simulating API error responses
#'
#' @param error_message Error message to return
#' @param status_code HTTP status code to return
#' @return List of mock functions for use with with_mock()
mock_api_error <- function(error_message = "Invalid request", status_code = 400) {
  list(
    `httr::POST` = function(...) {
      structure(list(status_code = status_code), class = "response")
    },
    `httr::content` = function(response) {
      list(error = list(message = error_message))
    }
  )
}

#' Test Function with API Error Mock
#'
#' Tests a function with a mocked error API response
#'
#' @param func Function to test
#' @param input Input to pass to the function
#' @param error_message Expected error message
#' @param status_code HTTP status code to return
test_with_api_error <- function(func, input, error_message = "Invalid request", status_code = 400) {
  mocks <- mock_api_error(error_message, status_code)
  with_mock(
    .env = mocks,
    {
      expect_error(func(input), paste0("Error: ", error_message))
    }
  )
}

#' Mock Invalid API Key Response
#'
#' Creates a list of mock functions for simulating invalid API key responses
#'
#' @return List of mock functions for use with with_mock()
mock_invalid_api_key <- function() {
  # Create a flag to track if the right environment variable was unset
  correct_var_unset <- FALSE

  list(
    `httr::POST` = function(...) {
      structure(list(status_code = 400), class = "response")
    },
    `httr::content` = function(response) {
      list(
        error = list(
          message = "API key not valid. Please pass a valid API key."
        )
      )
    },
    `Sys.unsetenv` = function(x) {
      # Check if any of the variables being unset is the one we're looking for
      if (any(x == "GEMINI_API_KEY")) {
        correct_var_unset <<- TRUE
      }
    },
    # Add a function to check if the key was unset
    check_key_unset = function() {
      expect_true(correct_var_unset, "GEMINI_API_KEY was not unset")
    }
  )
}

#' Test Function with Invalid API Key Mock
#'
#' Tests a function with a mocked invalid API key response
#'
#' @param func Function to test
#' @param input Input to pass to the function
test_with_invalid_api_key <- function(func, input) {
  # Save current API key and ensure it's restored after test
  original_api_key <- Sys.getenv("GEMINI_API_KEY")
  on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key))

  mocks <- mock_invalid_api_key()
  with_mock(
    .env = mocks[-length(mocks)], # Exclude the check function from mocks
    {
      # Check for correct error message
      expect_error(
        func(input),
        "Error: API key not valid. Please pass a valid API key."
      )

      # Call the check function to verify the key was unset
      mocks$check_key_unset()
    }
  )
}

#' Mock Rate Limit Response
#'
#' Creates a list of mock functions for simulating rate limiting scenarios
#'
#' @param response_text Text to return in the mock response
#' @return List of mock functions for use with with_mock()
mock_rate_limit <- function(response_text = "Rate-limited response") {
  # Set up request times to trigger rate limiting
  assign("request_times", rep(Sys.time(), 15), envir = .GlobalEnv)

  list(
    `httr::POST` = function(...) {
      structure(list(status_code = 200), class = "response")
    },
    `httr::content` = function(response) {
      list(
        candidates = list(
          list(
            content = list(
              parts = list(list(text = response_text))
            )
          )
        )
      )
    },
    `Sys.sleep` = function(x) {
      expect_gt(x, 0) # Ensure it actually waits
    }
  )
}

#' Test Function with Rate Limit Mock
#'
#' Tests a function's rate limiting behavior
#'
#' @param func Function to test
#' @param input Input to pass to the function
#' @param response_text Text to return in the mock response
test_with_rate_limit <- function(func, input, response_text = "Rate-limited response") {
  mocks <- mock_rate_limit(response_text)
  with_mock(
    .env = mocks,
    {
      result <- func(input)
      expect_type(result, "character")
      expect_equal(result, rep(response_text, length(input)))
      return(result)
    }
  )
}
