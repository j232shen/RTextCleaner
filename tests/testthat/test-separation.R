library(testthat)
library(httr)
library(mockery)
library(jsonlite)

#Text-only
test_input_1 <- c("How to run a java class inside of a container with testcontainers?")

test_that("separate_code_prompt works correctly", {
  # Mock API Response
  mock_response <- list(
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = '{"text": "How to run a java class inside of a container with testcontainers?", "code": ""}')
          ),
          role = "model"
        ),
        finishReason = "STOP",
        avgLogprobs = -5.42e-06
      )
    )
  )

  # Mock `httr::POST`
  stub(separate_code_prompt, "httr::POST", function(...) {
    structure(list(status_code = 200), class = "response")
  })

  # Mock `httr::content`
  stub(separate_code_prompt, "httr::content", function(...) mock_response)

  result <- separate_code_prompt(test_input_1)

  expected_result <- data.frame(
    text = "How to run a java class inside of a container with testcontainers?",
    code = "",
    stringsAsFactors = FALSE
  )


  expect_equal(result$text, expected_result$text)
  expect_equal(result$code, expected_result$code)
})





#Code-only
example_text <- c(
  "iob, ents = self._filter_coref_mismatches(iob, ents, prons)",
  "iob = self._fix_iob_seqs(iob)"
)


test_input_2 <- paste(example_text, collapse = "\n")

test_that("separate_code_prompt works correctly", {
  # Mock API Response
  mock_response <- list(
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = '{"text": "", "code": " iob, ents = self._filter_coref_mismatches(iob, ents, prons) iob = self._fix_iob_seqs(iob)"}')
          ),
          role = "model"
        ),
        finishReason = "STOP",
        avgLogprobs = -5.42e-06
      )
    )
  )

  # Mock `httr::POST`
  stub(separate_code_prompt, "httr::POST", function(...) {
    structure(list(status_code = 200), class = "response")
  })

  # Mock `httr::content`
  stub(separate_code_prompt, "httr::content", function(...) mock_response)

  result <- separate_code_prompt(test_input_2)

  expected_result <- data.frame(
    text = "",
    code = "iob, ents = self._filter_coref_mismatches(iob, ents, prons) iob = self._fix_iob_seqs(iob)",
    stringsAsFactors = FALSE
  )


  expect_equal(result$text, expected_result$text)
  expect_equal(result$code, expected_result$code)
})

# Mixed
example_text <- c("What is the benefit in using this approach:",
                  "```",
                  "otelAgent, err := NewInstance('otel-agent')",
                  "if err := wrapError(err, 'error creating otel-agent instance'); err != nil {",
                  "return nil, err",
                  "}",
                  "```")
test_input_3 <- paste(example_text, collapse = "\n")

xample_text <- c("What is the benefit in using this approach:",
                 "```",
                 "otelAgent, err := NewInstance('otel-agent')",
                 "if err := wrapError(err, 'error creating otel-agent instance'); err != nil {",
                 "return nil, err",
                 "}",
                 "```")
test_input_3 <- paste(example_text, collapse = "\n")

# Function to remove unnecessary line breaks
clean_text <- function(x) gsub("\\s+", " ", x)

test_that("separate_code_prompt works correctly", {
  # Mock API Response
  mock_response <- list(
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = '{"text": "What is the benefit in using this approach:", "code": "otelAgent, err := NewInstance(\'otel-agent\') if err := wrapError(err, \'error creating otel-agent instance\'); err != nil { return nil, err }"}')
          ),
          role = "model"
        ),
        finishReason = "STOP",
        avgLogprobs = -5.42e-06
      )
    )
  )

  # Mock `httr::POST`
  stub(separate_code_prompt, "httr::POST", function(...) {
    structure(list(status_code = 200), class = "response")
  })

  # Mock `httr::content`
  stub(separate_code_prompt, "httr::content", function(...) mock_response)

  # Call the function with correct test input
  result <- separate_code_prompt(test_input_3)


  expected_result <- data.frame(
    text = "What is the be\nefit i\n usi\ng this approach:" ,
    code = "otelAge\nt, err := NewI\nsta\nce('otel-age\nt') if err := wrapError(err, 'error creati\ng otel-age\nt i\nsta\nce'); err != \nil { retur\n \nil, err }",
    stringsAsFactors = FALSE
  )

  expect_equal(result$text, expected_result$text)
  expect_equal(result$code, expected_result$code)
})


#mixed-python code
test_input_python <- c("Here is a simple Python function:\n```python\ndef add(a, b):\n    return a + b\n```")

test_that("separate_code_prompt correctly handles Python code", {
  # Mock API Response
  mock_response <- list(
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = '{"text": "Here is a simple Python function:", "code": "def add(a, b):\\n    return a + b"}')
          ),
          role = "model"
        ),
        finishReason = "STOP"
      )
    )
  )

  # Mock `httr::POST`
  stub(separate_code_prompt, "httr::POST", function(...) {
    structure(list(status_code = 200), class = "response")
  })

  # Mock `httr::content`
  stub(separate_code_prompt, "httr::content", function(...) mock_response)

  result <- separate_code_prompt(test_input_python)

  expected_result <- data.frame(
    text = "Here is a simple Pytho\n fu\nctio\n:",
    code = "def add(a, b): retur\n a + b",
    stringsAsFactors = FALSE
  )

  expect_equal(result$text, expected_result$text)
  expect_equal(result$code, expected_result$code)
})



#Mixed- HTML

test_input_css <- c("This is how you define a simple CSS rule:\n```css\np { color: blue; font-size: 14px; }\n```")

test_that("separate_code_prompt correctly handles CSS code", {
  # Mock API Response
  mock_response <- list(
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = '{"text": "This is how you define a simple CSS rule:", "code": "p { color: blue; font-size: 14px; }"}')
          ),
          role = "model"
        ),
        finishReason = "STOP"
      )
    )
  )

  stub(separate_code_prompt, "httr::POST", function(...) {
    structure(list(status_code = 200), class = "response")
  })

  stub(separate_code_prompt, "httr::content", function(...) mock_response)

  result <- separate_code_prompt(test_input_css)

  expected_result <- data.frame(
    text = "This is how you defi\ne a simple CSS rule:",
    code = "p { color: blue; fo\nt-size: 14px; }",
    stringsAsFactors = FALSE
  )

  expect_equal(result$text, expected_result$text)
  expect_equal(result$code, expected_result$code)
})
