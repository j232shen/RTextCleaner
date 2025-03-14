#' @importFrom httr POST content content_type_json
#' @importFrom jsonlite fromJSON toJSON

library(httr)
library(jsonlite)


#' Remove Noise from Text Using Gemini API
#'
#' This function removes unwanted noise, excessive punctuation, and redundant characters from a given text.
#' @title Remove Noise from Text Using Gemini API
#' @name gemini_remove_noise
#' @param text_inputs A character vector containing text to be cleaned.
#' @param temperature A numeric value controlling response randomness (default: 1).
#' @param max_output_tokens Maximum number of tokens in the response (default: 1024).
#' @param api_key A character string for the API key (default: retrieved from environment variable `GEMINI_API_KEY`).
#' @param model The Gemini model version to use (default: "gemini-2.0-flash").
#'
#' @return A character vector of cleaned text.
#' @export
#' @examples
#' \dontrun{
#' text_samples <- c("Hiiii!!!    How are youuuu???", "OMG!!!!! This is soooooo coooool!!!")
#' print(gemini_remove_noise(text_samples))
#' }

gemini_remove_noise <- function(text_inputs,
                                temperature = 1,
                                max_output_tokens = 1024,
                                api_key = Sys.getenv("GEMINI_API_KEY"),
                                model = "gemini-2.0-flash") {

  # Validate API key
  if (nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }

  # Validate input type
  if (!is.character(text_inputs)) {
    stop("Error: text_inputs must be a character vector.")
  }

  # Check for empty input
  if (length(text_inputs) == 0) {
    stop("Error: text_inputs cannot be empty.")
  }

  model_query <- paste0(model, ":generateContent")
  responses <- character(length(text_inputs))

  for (idx in seq_along(text_inputs)) {
    text_input <- text_inputs[idx]

    # Define prompt
    full_prompt <- paste0(
      "TASK: Clean the following text by removing unwanted noise, special characters, and excessive punctuation. Ensure that the text remains readable and meaningful,
      INSTRUCTIONS:,
      - Remove repeated characters (e.g., 'sooo' -> 'so', 'yesss' -> 'yes').,
      - Remove unnecessary punctuation (e.g., '!!!', '???', '.....').,
      - Remove excessive whitespace (e.g., double spaces, extra line breaks).,
      - Preserve text structure, spacing, and readability.,
      - Do NOT change proper words, spelling, or alter sentence meaning.,
      STRICT RULES:,
      - Return ONLY the cleaned text.,
      - Do NOT include explanations or additional content.,
      - Ignore unrelated text-output only the processed version.,
      ===== INPUT BELOW =====",
      text_input
    )

    print(paste("Processing", idx, "of", length(text_inputs)))

    # Ensure we don't exceed 15 requests per minute
    if (exists("request_times", envir = .GlobalEnv)) {
      request_times <- get("request_times", envir = .GlobalEnv)
    } else {
      request_times <- numeric(0)
    }

    current_time <- Sys.time()
    if (length(request_times) == 15) {
      time_since_first_request <- as.numeric(difftime(current_time, request_times[1], units = "secs"))
      if (time_since_first_request < 60) {
        wait_time <- 60 - time_since_first_request
        print(paste("Rate limit reached. Waiting", round(wait_time, 2), "seconds..."))
        Sys.sleep(wait_time)
      }
    }

    # Make API request
    response <- httr::POST(
      url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
      query = list(key = api_key),
      httr::content_type_json(),
      encode = "json",
      body = list(
        contents = list(
          parts = list(
            list(text = full_prompt)
          )),
        generationConfig = list(
          temperature = temperature,
          maxOutputTokens = max_output_tokens
        )
      )
    )

    # Handle API response errors
    if (response$status_code != 200) {
      response_content <- httr::content(response)
      error_message <- if (!is.null(response_content$error$message)) response_content$error$message else "Unknown API error"
      stop(paste("Error -", error_message))
    }

    # Extract response
    response_content <- httr::content(response)
    if (is.null(response_content$candidates) || length(response_content$candidates) == 0 ||
        is.null(response_content$candidates[[1]]$content$parts[[1]]$text)) {
      stop("Error: Unexpected response format from API")
    }

    # Store cleaned text
    responses[idx] <- gsub("\n", "", response_content$candidates[[1]]$content$parts[[1]]$text)

    # Record request time
    request_times <- c(request_times, current_time)
    if (length(request_times) > 15) {
      request_times <- request_times[-1]
    }
    assign("request_times", request_times, envir = .GlobalEnv)
  }

  return(responses)
}


# text_samples <- c("OMG!!!!! This is soooooo coooool!!!", "Yesss!!!!    It's amaaazing...")
# clean_text <- gemini_remove_noise(text_samples)
# print(clean_text)
