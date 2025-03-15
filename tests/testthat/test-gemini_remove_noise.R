# library(mockery)
#
# test_that("gemini_remove_noise handles different responses for single input", {
#   # Define the correct mock API response
#   mock_response <- list(
#     candidates = list(
#       list(content = list(parts = list(list(text = "Hi!"))))  # Expected cleaned text
#     )
#   )
#
#   # Mock `httr::POST` to always return a successful response
#   stub(gemini_remove_noise, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))
#
#   # Mock `httr::content` to return our predefined response
#   stub(gemini_remove_noise, "httr::content", function(...) mock_response)
#
#   # Run function with test input
#   result <- gemini_remove_noise(c("Hiiii!!!    "))
#
#   # Validate output
#   expect_equal(result, c("Hi!"))  # Expected output must match
# })
#
#
#
# test_that("gemini_remove_noise handles different responses for multiple inputs", {
#   # Define mock response sequence
#   mock_response_1 <- list(
#     candidates = list(
#       list(content = list(parts = list(list(text = "OMG! This is so cool!"))))
#     )
#   )
#
#   mock_response_2 <- list(
#     candidates = list(
#       list(content = list(parts = list(list(text = "Yes! It's amazing."))))
#     )
#   )
#
#   # Use stub() to mock `httr::POST` and `httr::content`
#   stub(gemini_remove_noise, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))
#
#   # Use a counter to switch between mock responses
#   response_counter <- 0
#   stub(gemini_remove_noise, "httr::content", function(...) {
#     response_counter <<- response_counter + 1
#     if (response_counter == 1) return(mock_response_1)
#     else return(mock_response_2)
#   })
#
#   # Run function with test input
#   result <- gemini_remove_noise(c("OMG!!!!! This is soooooo coooool!!!", "Yesss!!!!    It's amaaazing..."))
#
#   # Validate outputs
#   expect_equal(result, c("OMG! This is so cool!", "Yes! It's amazing."))
# })
#
#
#
# test_that("gemini_remove_noise handles different responses for multiple inputs", {
#   # Define mock response sequence
#   mock_response_1 <- list(
#     candidates = list(
#       list(content = list(parts = list(list(text = "OMG! This is so cool!"))))
#     )
#   )
#
#   mock_response_2 <- list(
#     candidates = list(
#       list(content = list(parts = list(list(text = "Yes! It's amazing."))))
#     )
#   )
#
#   # Use stub() to mock `httr::POST` and `httr::content`
#   stub(gemini_remove_noise, "httr::POST", function(...) structure(list(status_code = 200), class = "response"))
#
#   # Use a counter to switch between mock responses
#   response_counter <- 0
#   stub(gemini_remove_noise, "httr::content", function(...) {
#     response_counter <<- response_counter + 1
#     if (response_counter == 1) return(mock_response_1)
#     else return(mock_response_2)
#   })
#
#   # Run function with test input
#   result <- gemini_remove_noise(c("OM^&$G!!!! Th&%is is sooo)*ooo cooo#$ool!!!", "Yes$%ss!!!!    It's amaaa*zing..."))
#
#   # Validate outputs
#   expect_equal(result, c("OMG! This is so cool!", "Yes! It's amazing."))
# })
#
#
#
# test_that("gemini_remove_noise handles invalid API key error", {
#   original_api_key <- Sys.getenv("GEMINI_API_KEY") # save current API key
#   on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key)) # restore API key after test
#
#   correct_var_unset <- FALSE # flag to track if the right environment variable was unset
#
#   with_mock(
#     # mock a HTTP response failure (status 400)
#     `httr::POST` = mock_post_failure(),
#
#     # mock HTTP response error message to be due to invalid key
#     `httr::content` = mock_content_error("API key not valid. Please pass a valid API key."),
#
#     # mock unset all environment variables due to invalid key
#     `Sys.unsetenv` = function(x) {
#       if (any(x == "GEMINI_API_KEY")) { # toggle flag if GEMINI_API_KEY was unset
#         correct_var_unset <<- TRUE
#       }
#     },
#     {
#       # check if function raises error correctly using HTTP error message
#       expect_error(
#         gemini_remove_noise(c("test input")),
#         "Error: API key not valid. Please pass a valid API key."
#       )
#       # verify that GEMINI_API_KEY was unset
#       expect_true(correct_var_unset, "GEMINI_API_KEY was not unset")
#     }
#   )
# })
#
#
#
# test_that("gemini_remove_noise throws an error for empty input", {
#   expect_error(gemini_remove_noise(character(0)), "Error: text_inputs cannot be empty")
# })
#
#
#
# test_that("gemini_remove_noise throws an error for non-character input", {
#   expect_error(gemini_remove_noise(42), "Error: text_inputs must be a character vector.")
#   expect_error(gemini_remove_noise(list("valid", 123)), "Error: text_inputs must be a character vector.")
#   expect_error(gemini_remove_noise(TRUE), "Error: text_inputs must be a character vector.")
# })
