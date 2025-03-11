library(httr)
library(jsonlite)

# Initialize deque to track request times
assign("request_times", numeric(0), envir = .GlobalEnv)

#' Clean Text Using Gemini API
#'
#' This function cleans a column of text inputs by removing noise, special characters,
#' and excessive punctuation while preserving readability and meaning.
#'
#' @param text_inputs A character vector containing text to be cleaned.
#' @param temperature A numeric value controlling response randomness (default: 1).
#' @param max_output_tokens Maximum number of tokens in the response (default: 1024).
#' @param api_key A character string for the API key (default: retrieved from environment variable `GEMINI_API_KEY`).
#' @param model The Gemini model version to use (default: "gemini-2.0-flash").
#'
#' @return A character vector of the same length as `text_inputs`, containing the cleaned text.
#' @export
#'
#' @examples
#' \dontrun{
#' text_samples <- c("Hiiii!!!    How are youuuu???", "OMG!!!!! This is soooooo coooool!!!")
#' clean_text <- gemini_remove_noise(text_samples)
#' print(clean_text)
#' }

gemini_remove_noise <- function(text_inputs,
                                temperature = 1,
                                max_output_tokens = 1024,
                                api_key = Sys.getenv("GEMINI_API_KEY"),
                                model = "gemini-2.0-flash") {
  
  if (nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  # Create an empty character vector of the same length as input
  responses <- character(length(text_inputs))
  
  for (idx in seq_along(text_inputs)) {
    text_input <- text_inputs[idx]
    full_prompt <- paste0(
      "TASK: Clean the following text by removing unwanted noise, special characters, and excessive punctuation. Ensure that the text remains readable and meaningful.\n\n",
      "INSTRUCTIONS:\n",
      "- Remove repeated characters (e.g., 'sooo' → 'so', 'yesss' → 'yes').\n",
      "- Remove unnecessary punctuation (e.g., '!!!', '???', '.....').\n",
      "- Remove excessive whitespace (e.g., double spaces, extra line breaks).\n",
      "- Preserve text structure, spacing, and readability.\n",
      "- Do NOT change proper words, spelling, or alter sentence meaning.\n\n",
      "STRICT RULES:\n",
      "- Return ONLY the cleaned text.\n",
      "- Do NOT include explanations or additional content.\n",
      "- Ignore unrelated text—output only the processed version.\n\n",
      "===== INPUT BELOW =====\n\n",
      text_input
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
    response <- POST(
      url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
      query = list(key = api_key),
      content_type_json(),
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
      response_text <- gsub("\n", "", candidate$content$parts[[1]]$text) # Remove newlines
      responses[idx] <- response_text
    }
    
    # Record the timestamp of the request
    request_times <<- c(request_times, current_time)
    if (length(request_times) > 15) {
      request_times <<- request_times[-1]
    }
  }
  
  return(responses)
}

# Test the function
# text_input <- "Hiiii!!!    How are youuuu???      "
# cat(gemini_remove_noise(text_input))

