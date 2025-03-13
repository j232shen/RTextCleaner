# File: tests/testthat/helper-mock_api.R
# Helper functions for mocking API interactions in tests

#' Create a mock successful POST response
#'
#' @param status_code HTTP status code to return (default: 200)
#' @return A function that can be used to mock httr::POST
mock_post_success <- function(status_code = 200) {
  function(...) {
    structure(list(status_code = status_code), class = "response")
  }
}

#' Create a mock failed POST response
#'
#' @param status_code HTTP status code to return (default: 400)
#' @return A function that can be used to mock httr::POST
mock_post_failure <- function(status_code = 400) {
  function(...) {
    structure(list(status_code = status_code), class = "response")
  }
}

#' Create a mock content function that returns a successful response
#'
#' @param text The text to return in the summary
#' @return A function that can be used to mock httr::content
mock_content_success <- function(text = "Summary text") {
  function(response) {
    list(
      candidates = list(
        list(
          content = list(
            parts = list(list(text = text))
          )
        )
      )
    )
  }
}

#' Create a mock content function that returns different responses for each call
#'
#' @param texts A vector of texts to return in sequence
#' @return A function that can be used to mock httr::content
mock_content_sequence <- function(texts = c("Output 1", "Output 2")) {
  call_count <- 0
  function(response) {
    call_count <<- call_count + 1
    text_index <- min(call_count, length(texts))
    list(
      candidates = list(
        list(
          content = list(
            parts = list(list(text = texts[text_index]))
          )
        )
      )
    )
  }
}

#' Create a mock content function that returns an error response
#'
#' @param error_message The error message to return
#' @return A function that can be used to mock httr::content
mock_content_error <- function(error_message = "Invalid request") {
  function(response) {
    list(error = list(message = error_message))
  }
}

#' Mock function for API key prompt
#'
#' @param api_key The API key to return when prompted
#' @return A function that can be used to mock readline
mock_api_key_prompt <- function(api_key = "fake_test_api_key") {
  function(prompt) {
    return(api_key)
  }
}

#' Mock function for Sys.sleep to test rate limiting
#'
#' @return A function that can be used to mock Sys.sleep
mock_sleep_fn <- function() {
  function(x) {
    # Do nothing, just prevent actual sleeping
    testthat::expect_gt(x, 0) # ensure it actually waits
  }
}

#' Set up mocked rate limiting environment
#'
#' @return Invisibly returns the previous request_times value
setup_rate_limit_test <- function() {
  old_times <- if (exists("request_times", envir = .GlobalEnv)) {
    get("request_times", envir = .GlobalEnv)
  } else {
    numeric(0)
  }

  assign("request_times", rep(Sys.time(), 15), envir = .GlobalEnv)

  invisible(old_times)
}
