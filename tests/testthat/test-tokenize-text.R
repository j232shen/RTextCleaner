# test_that("tokenize_text correctly processes simple text", {
#   result <- tokenize_text("Hello world")
#   expect_equal(result, c("hello", "world"))
# })
#
# test_that("tokenize_text handles uppercase and lowercase", {
#   result <- tokenize_text("Hello WORLD")
#   expect_equal(result, c("hello", "world"))
# })
#
# test_that("tokenize_text removes punctuation", {
#   result <- tokenize_text("Hello, world! How are you?")
#   expect_equal(result, c("hello", "world", "how", "are", "you"))
# })
#
# test_that("tokenize_text removes numbers", {
#   result <- tokenize_text("I have 3 apples and 5 oranges")
#   expect_equal(result, c("i", "have", "apples", "and", "oranges"))
# })
#
# test_that("tokenize_text handles multiple spaces", {
#   result <- tokenize_text("hello   world")
#   expect_equal(result, c("hello", "world"))
# })
#
# test_that("tokenize_text handles multiple input texts", {
#   result <- tokenize_text(c("Hello world", "Testing 123"))
#   expect_equal(result, c("hello", "world", "testing"))
# })
#
# test_that("tokenize_text handles empty input", {
#   expect_error(tokenize_text(character(0)))
# })
#
# test_that("tokenize_text handles NULL input", {
#   expect_error(tokenize_text(NULL))
# })
#
# test_that("tokenize_text handles NA values", {
#   result <- tokenize_text(c("Hello", NA, "world"))
#   expect_equal(result, c("hello", "world"))
# })
#
# test_that("tokenize_text handles empty strings", {
#   result <- tokenize_text(c("Hello", "", "world"))
#   expect_equal(result, c("hello", "world"))
# })
#
# test_that("tokenize_text handles special characters", {
#   result <- tokenize_text("special % ^ & * characters")
#   expect_equal(result, c("special", "characters"))
# })
#
# test_that("tokenize_text handles mixed content", {
#   result <- tokenize_text("123 abc !@# DEF")
#   expect_equal(result, c("abc", "def"))
# })
