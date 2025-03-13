library(testthat)
library(httr)

test_input_1 <- c("The quick brown fox jumps over the lazy dog.")
test_input_2 <- rep(test_input_1, times = 2)

# test_that("summarize returns a character vector with correct length", {
#   with_mock(
#     `httr::content` = function(response) {
#       # Return the mock content that summarize() expects
#       list(
#         candidates = list(
#           list(
#             content = list(
#               parts = list(list(text = "Summary text"))
#             )
#           )
#         )
#       )
#     },
#     {
#       # Call your summarize function
#       result <- summarize(test_input_2)
#
#       # Check result length and type
#       expect_length(result, 2)
#       expect_type(result, "character")
#
#       # Ensure that it matches the expected summary text
#       expect_equal(result, c("Summary text", "Summary text"))
#     }
#   )
# })

# test_that("summarize handles invalid API key and prompts for new key", {
#   original_api_key <- Sys.getenv("GEMINI_API_KEY")  # Store the original API key
#
#   with_mock(
#     # Mock the content function to simulate an invalid API key error
#     `httr::content` = function(response) {
#       list(
#         error = list(
#           message = "API key not valid. Please pass a valid API key."
#         )
#       )
#     },
#     {
#       expect_error(summarize(test_input_1),"Error - API key not valid. Please pass a valid API key.")
#     }
#   )
# })

# test_that("summarize throws an error on API failure", {
#   with_mock(
#     `httr::POST` = function(...) {
#       structure(
#         list(
#           status_code = 400,
#           content = list(error = list(message = "Invalid request"))
#         ),
#         class = "response"
#       )
#     },
#     {
#       expect_error(summarize(test_input), "Error: Invalid request")
#     }
#   )
# })
#
# test_that("summarize waits when rate limit is reached", {
#   request_times <<- rep(Sys.time(), 15)
#
#   with_mock(
#     `httr::POST` = function(...) {
#       structure(
#         list(
#           status_code = 200,
#           content = list(
#             candidates = list(list(content = list(parts = list(list(text = "Rate-limited response")))))
#           )
#         ),
#         class = "response"
#       )
#     },
#     `Sys.sleep` = function(x) {
#       expect_gt(x, 0)  # Ensure it actually waits
#     },
#     {
#       result <- summarize(test_input)
#       expect_type(result, "character")
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
