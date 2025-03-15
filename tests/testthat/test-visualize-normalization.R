# mock helper function to test get_last_normalization
mock_get_last_normalization <- function(original_text = NULL, normalized_text = NULL) {
  function() {
    if (is.null(original_text) || is.null(normalized_text)) {
      return(NULL)
    }
    return(list(
      original = original_text,
      normalized = normalized_text
    ))
  }
}

# helper function to create test data
create_test_data <- function() {
  original <- c(
    "i luv txt msgs",
    "gr8 to c u",
    "The quick brown fox jumped over the lazy dog.",
    "Teh quik brwn fox jumps ovr da lzy dog."
  )

  normalized <- c(
    "i love text messages",
    "great to see you",
    "The quick brown fox jumped over the lazy dog.",
    "The quick brown fox jumps over the lazy dog."
  )

  return(list(original = original, normalized = normalized))
}

test_that("visualize_normalization returns a ggplot object", {
  test_data <- create_test_data()

  result <- visualize_normalization(
    original_text = test_data$original,
    normalized_text = test_data$normalized
  )

  expect_true(inherits(result, "ggplot"))
})

test_that("visualize_normalization handles case with no changes", {
  original <- c("Hello world", "Testing", "No changes")
  normalized <- c("Hello world", "Testing", "No changes")

  result <- visualize_normalization(
    original_text = original,
    normalized_text = normalized
  )

  expect_true(inherits(result, "ggplot"))
  # Check if it's a ggplot with a text annotation (no changes message)
  expect_true(any(sapply(result$layers, function(l) inherits(l$geom, "GeomText"))))
})

test_that("visualize_normalization handles top_n parameter", {
  test_data <- create_test_data()

  result <- visualize_normalization(
    top_n = 5,
    original_text = test_data$original,
    normalized_text = test_data$normalized
  )

  expect_true(inherits(result, "ggplot"))
})

test_that("visualize_normalization uses last normalization when no text provided", {
  test_data <- create_test_data()

  with_mock(
    `RTextCleaner:::get_last_normalization` = mock_get_last_normalization(
      test_data$original, test_data$normalized
    ),
    {
      result <- visualize_normalization()
      expect_true(inherits(result, "ggplot"))
    }
  )
})

test_that("visualize_normalization throws error when no normalization data available", {
  with_mock(
    `RTextCleaner:::get_last_normalization` = mock_get_last_normalization(),
    {
      expect_error(
        visualize_normalization(),
        "No normalization data found. Please run normalization first or provide text vectors."
      )
    }
  )
})

test_that("visualize_normalization handles empty input", {
  expect_error(
    visualize_normalization(original_text = character(0), normalized_text = character(0))
  )
})

test_that("visualize_normalization handles mismatched input lengths", {
  original <- c("text1", "text2")
  normalized <- c("modified1")

  expect_error(
    visualize_normalization(original_text = original, normalized_text = normalized)
  )
})

test_that("visualize_normalization handles non-character vectors", {
  numeric_vector <- c(1234, 5678)
  character_vector <- c("text1", "text2")

  expect_error(
    visualize_normalization(original_text = numeric_vector, normalized_text = character_vector)
  )
  expect_error(
    visualize_normalization(original_text = character_vector, normalized_text = numeric_vector)
  )
})
