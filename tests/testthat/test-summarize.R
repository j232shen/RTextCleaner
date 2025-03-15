# test_input_1 <- c("The quick brown fox jumps over the lazy dog.")
# test_input_2 <- rep(test_input_1, times = 2)
#
# test_that("summarize returns a character vector with correct length", {
#   with_mock(
#     # mock a successful HTTP response (status 200)
#     `httr::POST` = mock_post_success(),
#
#     # mock the response content to return same dummy text for every API call
#     `httr::content` = mock_content_success("Summary text"),
#
#     {
#       result <- summarize(test_input_2)
#
#       # check if the number of outputs matches the number of inputs
#       expect_length(result, 2)
#
#       # check if the result vector has character type
#       expect_type(result, "character")
#
#       # check if each input produced the expected dummy response in each output
#       expect_equal(result, c("Summary text", "Summary text"))
#     }
#   )
# })
#
# test_that("summarize handles different responses for multiple inputs", {
#   with_mock(
#     # mock a successful HTTP response (status 200)
#     `httr::POST` = mock_post_success(),
#
#     # mock the response content to return DIFFERENT dummy text for every API call
#     `httr::content` = mock_content_sequence(c("Summary 1", "Summary 2")),
#
#     {
#       result <- summarize(c("Text 1", "Text 2"))
#       expect_equal(result, c("Summary 1", "Summary 2")) # check if each output matches its corresponding dummy text
#     }
#   )
# })
#
# test_that("summarize prompts for API key when none is available", {
#   # store original API key
#   original_api_key <- Sys.getenv("GEMINI_API_KEY")
#
#   # clear the API key from environment variables to simulate a user without a key
#   Sys.unsetenv("GEMINI_API_KEY")
#
#   # restore API key after test ends
#   on.exit(Sys.setenv(GEMINI_API_KEY = original_api_key))
#
#   with_mock(
#     # mock the readline function to simulate user inputting "dummy_key" when prompted
#     `readline` = mock_api_key_prompt("dummy_key"),
#
#     # mock a successful HTTP response when the API is called with "dummy_key"
#     `httr::POST` = mock_post_success(),
#
#     # mock the response content to return dummy text
#     `httr::content` = mock_content_success("Summary"),
#     {
#       result <- summarize(c("Test text"))
#       expect_equal(result, c("Summary"))
#     }
#   )
# })
#
# test_that("summarize handles invalid API key error", {
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
#         summarize(test_input_1),
#         "Error: API key not valid. Please pass a valid API key."
#       )
#       # verify that GEMINI_API_KEY was unset
#       expect_true(correct_var_unset, "GEMINI_API_KEY was not unset")
#     }
#   )
# })
#
# test_that("summarize throws an error on API failure", {
#   with_mock(
#     # mock a HTTP response failure (status 400)
#     `httr::POST` = mock_post_failure(),
#
#     # mock HTTP response error message to be due to invalid request
#     `httr::content` = mock_content_error("Invalid request"),
#
#     {
#       # check if function raises error correctly using HTTP error message
#       expect_error(summarize(test_input_1), "Error: Invalid request")
#     }
#   )
# })
#
# test_that("summarize waits when rate limit is reached", {
#   # set up test environment where we simulate having made 15 requests already
#   old_times <- setup_rate_limit_test()
#
#   # restore the original request times when test ends
#   on.exit(rate_limit_env$request_times <- old_times)
#
#   with_mock(
#     # mock a successful HTTP response (status 200)
#     `httr::POST` = mock_post_success(),
#
#     # mock the response content to return dummy text
#     `httr::content` = mock_content_success("Rate-limited response"),
#
#     # mock the sleep function to verify it's called with a positive duration and confirm waiting
#     `Sys.sleep` = mock_sleep_fn(),
#
#     {
#       # call summarize to trigger the rate limiting logic
#       result <- summarize(test_input_1)
#
#       # verify expected result after waiting
#       expect_type(result, "character")
#       expect_equal(result, c("Rate-limited response"))
#     }
#   )
# })
#
# test_that("summarize throws an error for empty input", {
#   expect_error(summarize(character(0)), "Error: text_inputs cannot be empty")
# })
#
# test_that("summarize throws an error for non-character input", {
#   expect_error(summarize(42), "Error: text_inputs must be a character vector.")
#   expect_error(summarize(list("valid", 123)), "Error: text_inputs must be a character vector.")
#   expect_error(summarize(TRUE), "Error: text_inputs must be a character vector.")
# })
