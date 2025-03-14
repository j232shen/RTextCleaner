library(testthat)
library(httr)
library(mockery)
library(jsonlite)

test_that("gemini_text_normalization handles different responses for single input", {
  mock_response <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Oh my god, this is the best day ever!"))))
    )
  )

  stub(gemini_text_normalization, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))
  stub(gemini_text_normalization, "httr::content", function(...) mock_response)

  result <- gemini_text_normalization(c("omg dis is da best day evr!!!"))
  expect_equal(result, c("Oh my god, this is the best day ever!"))
})



test_that("gemini_text_normalization handles different responses for multiple inputs", {
  # Define mock response sequence
  mock_response_1 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "See you later, I have to go to school."))))
    )
  )

  mock_response_2 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Oh my god, this is the best day ever!!!"))))
    )
  )

  # Use stub() to mock `httr::POST` and `httr::content`
  stub(gemini_text_normalization, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))

  # Use a counter to switch between mock responses
  response_counter <- 0
  stub(gemini_text_normalization, "httr::content", function(...) {
    response_counter <<- response_counter + 1
    if (response_counter == 1) return(mock_response_1)
    else return(mock_response_2)
  })

  # Run function with test input
  result <- gemini_text_normalization(c("c u l8r, i gotta go 2 skool.", "omg dis is da best day evr!!!"))

  # Validate outputs
  expect_equal(result, c("See you later, I have to go to school.", "Oh my god, this is the best day ever!!!"))
})



test_that("gemini_text_normalization handles invalid API key error", {
  original_api_key <- Sys.getenv("GEMINI_API_KEY")
  on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key))

  Sys.setenv(GEMINI_API_KEY = "fake_api_key")

  stub(gemini_text_normalization, "httr::POST", function(...) {
    structure(list(status_code = 400), class = "response")
  })

  stub(gemini_text_normalization, "httr::content", function(...) {
    list(error = list(message = "API key not valid. Please pass a valid API key."))
  })

  expect_error(
    gemini_text_normalization(c("Test input")),
    regexp = "API key not valid"
  )
})




test_that("gemini_text_normalization throws an error for empty input", {
  expect_error(gemini_text_normalization(character(0)), "Error: text_inputs cannot be empty")
})



test_that("gemini_text_normalization throws an error for non-character input", {
  expect_error(gemini_text_normalization(42), "Error: text_inputs must be a character vector.")
  expect_error(gemini_text_normalization(list("valid", 123)), "Error: text_inputs must be a character vector.")
  expect_error(gemini_text_normalization(TRUE), "Error: text_inputs must be a character vector.")
})


