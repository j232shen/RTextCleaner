library(testthat)
library(httr)

test_that("gemini_remove_noise handles different responses for single input", {
  # Define the correct mock API response
  mock_response <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Hi!"))))  # Expected cleaned text
    )
  )

  # Mock `httr::POST` to always return a successful response
  stub(gemini_remove_noise, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))

  # Mock `httr::content` to return our predefined response
  stub(gemini_remove_noise, "httr::content", function(...) mock_response)

  # Run function with test input
  result <- gemini_remove_noise(c("Hiiii!!!    "))

  # Validate output
  expect_equal(result, c("Hi!"))  # Expected output must match
})



test_that("gemini_remove_noise handles different responses for multiple inputs", {
  # Define mock response sequence
  mock_response_1 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "OMG! This is so cool!"))))
    )
  )

  mock_response_2 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Yes! It's amazing."))))
    )
  )

  # Use stub() to mock `httr::POST` and `httr::content`
  stub(gemini_remove_noise, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))

  # Use a counter to switch between mock responses
  response_counter <- 0
  stub(gemini_remove_noise, "httr::content", function(...) {
    response_counter <<- response_counter + 1
    if (response_counter == 1) return(mock_response_1)
    else return(mock_response_2)
  })

  # Run function with test input
  result <- gemini_remove_noise(c("OMG!!!!! This is soooooo coooool!!!", "Yesss!!!!    It's amaaazing..."))

  # Validate outputs
  expect_equal(result, c("OMG! This is so cool!", "Yes! It's amazing."))
})



test_that("gemini_remove_noise handles different responses for multiple inputs", {
  # Define mock response sequence
  mock_response_1 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "OMG! This is so cool!"))))
    )
  )

  mock_response_2 <- list(
    candidates = list(
      list(content = list(parts = list(list(text = "Yes! It's amazing."))))
    )
  )

  # Use stub() to mock `httr::POST` and `httr::content`
  stub(gemini_remove_noise, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))

  # Use a counter to switch between mock responses
  response_counter <- 0
  stub(gemini_remove_noise, "httr::content", function(...) {
    response_counter <<- response_counter + 1
    if (response_counter == 1) return(mock_response_1)
    else return(mock_response_2)
  })

  # Run function with test input
  result <- gemini_remove_noise(c("OM^&$G!!!! Th&%is is sooo)*ooo cooo#$ool!!!", "Yes$%ss!!!!    It's amaaa*zing..."))

  # Validate outputs
  expect_equal(result, c("OMG! This is so cool!", "Yes! It's amazing."))
})



test_that("gemini_remove_noise handles invalid API key error", {
  original_api_key <- Sys.getenv("GEMINI_API_KEY")
  on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key))

  Sys.unsetenv("GEMINI_API_KEY")

  stub(gemini_remove_noise, "httr::POST", function(...) {
    structure(list(status_code = 400), class = "response")
  })

  stub(gemini_remove_noise, "httr::content", function(...) {
    list(error = list(message = "API key not valid. Please pass a valid API key."))
  })

  expect_error(
    gemini_remove_noise(c("Test input")),
    regexp = "API key not valid"
  )
})



test_that("gemini_remove_noise throws an error for empty input", {
  expect_error(gemini_remove_noise(character(0)), "Error: text_inputs cannot be empty")
})



test_that("gemini_remove_noise throws an error for non-character input", {
  expect_error(gemini_remove_noise(42), "Error: text_inputs must be a character vector.")
  expect_error(gemini_remove_noise(list("valid", 123)), "Error: text_inputs must be a character vector.")
  expect_error(gemini_remove_noise(TRUE), "Error: text_inputs must be a character vector.")
})
