library(httr)
library(jsonlite)

# Initialize deque to track request times
assign("request_times", numeric(0), envir = .GlobalEnv)

#' Normalize Text Using Gemini API
#'
#' This function normalizes a column of text inputs by correcting capitalization, fixing spelling errors,
#' and expanding informal abbreviations while maintaining readability and meaning.
#'
#' @name gemini_text_normalization
#' @param text_inputs A character vector containing text to be normalized.
#' @param temperature A numeric value controlling response randomness (default: 1).
#' @param max_output_tokens Maximum number of tokens in the response (default: 1024).
#' @param api_key A character string for the API key (default: retrieved from environment variable `GEMINI_API_KEY`).
#' @param model The Gemini model version to use (default: "gemini-2.0-flash").
#'
#' @return A character vector of the same length as `text_inputs`, containing the normalized text.
#' @export
#'
#' @examples
#'
#' text_samples <- c("omg dis is da best day evr!!!", "yayyy, tysm this is sooo gr8!!!")
#' normalized_text <- gemini_text_normalization(text_samples)
#' print(normalized_text)

# gemini_text_normalization
normalize <- function(text_inputs,
                                      temperature = 1,
                                      max_output_tokens = 1024,
                                      api_key = Sys.getenv("GEMINI_API_KEY"),
                                      model = "gemini-2.0-flash") {

  if (nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }

  model_query <- paste0(model, ":generateContent")

  responses <- character(length(text_inputs))

  for (idx in seq_along(text_inputs)) {
    text_input <- text_inputs[idx]
    full_prompt <- paste0(
      "TASK: Normalize the following text by fixing capitalization, correcting spelling errors, and expanding informal abbreviations while keeping the meaning intact.

      INSTRUCTIONS:
      - Capitalize sentence beginnings and proper nouns.
      - Fix common spelling mistakes (e.g., 'beleive' -> 'believe', 'recieve' -> 'receive').
      - Expand abbreviations and informal slang (e.g., 'u' -> 'you', 'gr8' -> 'great', 'thx' -> 'thanks').
      - Ensure correct grammar and readability while keeping the original meaning.
      - Keep repeated characters, punctuation, excessive whitespace or change proper names unless necessary.

      STRICT RULES:
      - Return ONLY the corrected text.
      - Do NOT include explanations or additional content.
      - Ignore unrelated text-output only the processed version.

      Example 1:
      Input: i cant beleive u did that! thx 4 da help
      Output: I can't believe you did that! Thanks for the help.

      Example 2:
      Input: omg dis is da best day evr!!!
      Output: Oh my god, this is the best day ever!!!

      Example 3:
      Input: c u l8r, i gotta go 2 skool.
      Output: See you later, I have to go to school.

      ===== INPUT BELOW =====
      ", text_input
    )

    print(paste("Processing", idx, "of", length(text_inputs)))

    # Ensure we don't exceed 15 requests per minute
    current_time <- Sys.time()
    if (length(request_times) == 15) {
      time_since_first_request <- as.numeric(difftime(current_time, request_times[1], units = "secs"))
      if (time_since_first_request < 60) {
        wait_time <- 60 - time_since_first_request
        print(paste("Rate limit reached. Waiting", round(wait_time, 2), "seconds..."))
        Sys.sleep(wait_time)
      }
    }

    # make the request
    response <- httr::POST(
      url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
      query = list(key = api_key),
      httr::content_type_json(),
      encode = "json",
      body = list(
        contents = list(
          parts = list(
            list(text = full_prompt) # prompt integrates user input
          )),
        generationConfig = list(
          temperature = temperature,
          maxOutputTokens = max_output_tokens
        )
      )
    )

    if (response$status_code != 200) {
      print(paste("Error - Status Code:", response$status_code))
      print(content(response))
      stop(paste("Error - ", content(response)$error$message))
    }

    candidates <- content(response)$candidates

    for (candidate in candidates) {
      response_text <- gsub("\n", "", candidate$content$parts[[1]]$text)
      responses[idx] <- response_text
    }

    # Record the timestamp of the request
    request_times <<- c(request_times, current_time)
    if (length(request_times) > 15) {
      request_times <<- request_times[-1]
    }
  }

  store_normalization_result(text_inputs, responses) # store in environment for visualization

  return(responses)
}


# text_input <- c("omg dis is da best day evr!!!", "yayyy, tysm this is sooo gr8!!! i cant beleive their is only twwo dayz left!!!!!")
# gemini_text_normalization(text_input)
