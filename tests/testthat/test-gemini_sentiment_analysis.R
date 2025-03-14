library(testthat)
library(httr)
library(mockery)
library(jsonlite)

test_input_1 <- c("I love pizza, sushi, and burgers!")
test_input_2 <- c("I hate horror movies. They're terrible.", "My top 3 movies are Inception, The Matrix, and Interstellar.")

test_that("gemini_sentiment_analysis handles different responses for single input", {
  mock_response <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Positive"))))
    )
  )

  stub(gemini_sentiment_analysis, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))
  stub(gemini_sentiment_analysis, "httr::content", function(...) mock_response)

  result <- gemini_sentiment_analysis(test_input_1)
  expect_equal(result, c("Positive"))
})


test_that("gemini_sentiment_analysis handles different responses for multiple inputs", {
  mock_response_1 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Negative"))))
    )
  )

  mock_response_2 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Positive"))))
    )
  )

  stub(gemini_sentiment_analysis, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))

  response_counter <- 0
  stub(gemini_sentiment_analysis, "httr::content", function(...) {
    response_counter <<- response_counter + 1
    if (response_counter == 1) {
      return(mock_response_1)
    } else {
      return(mock_response_2)
    }
  })

  result <- gemini_sentiment_analysis(test_input_2)
  expect_equal(result, c("Negative", "Positive"))
})



test_that("gemini_sentiment_analysis handles invalid API key error", {
  original_api_key <- Sys.getenv("GEMINI_API_KEY")
  on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key))

  Sys.setenv(GEMINI_API_KEY = "fake_api_key")

  stub(gemini_sentiment_analysis, "httr::POST", function(...) {
    structure(list(status_code = 400), class = "response")
  })

  stub(gemini_sentiment_analysis, "httr::content", function(...) {
    list(error = list(message = "API key not valid. Please pass a valid API key."))
  })

  expect_error(
    gemini_sentiment_analysis(c("Test input")),
    regexp = "API key not valid"
  )
})



test_that("gemini_sentiment_analysis throws an error for empty input", {
  expect_error(gemini_sentiment_analysis(character(0)), "Error: text_inputs cannot be empty")
})



test_that("gemini_sentiment_analysis throws an error for non-character input", {
  expect_error(gemini_sentiment_analysis(42), "Error: text_inputs must be a character vector.")
  expect_error(gemini_sentiment_analysis(list("valid", 123)), "Error: text_inputs must be a character vector.")
  expect_error(gemini_sentiment_analysis(TRUE), "Error: text_inputs must be a character vector.")
})

