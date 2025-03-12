#' @importFrom httr POST content content_type_json
#' @importFrom jsonlite fromJSON toJSON

library(httr)
library(jsonlite)


#' Sentiment Analysis Using Gemini API
#'
#' This function analyzes the sentiment of text inputs and classifies them as Positive, Neutral, or Negative.
#' @title Sentiment Analysis Using Gemini API
#' @name gemini_sentiment_analysis
#' @param text_inputs A character vector containing text to be analyzed.
#' @param temperature A numeric value controlling response randomness (default: 1).
#' @param max_output_tokens Maximum number of tokens in the response (default: 1024).
#' @param api_key A character string for the API key (default: retrieved from environment variable `GEMINI_API_KEY`).
#' @param model The Gemini model version to use (default: "gemini-2.0-flash").
#'
#' @return A character vector of the same length as `text_inputs`, containing sentiment labels (`Positive`, `Neutral`, or `Negative`).
#' @export
#'
#' @examples
#'
#' text_samples <- c("I love pizza, sushi, and burgers!", "I hate horror movies. They're terrible.")
#' sentiment_results <- gemini_sentiment_analysis(text_samples)
#' print(sentiment_results)
#'

gemini_sentiment_analysis <- function(text_inputs,
                                      temperature = 1,
                                      max_output_tokens = 1024,
                                      api_key = Sys.getenv("GEMINI_API_KEY"),
                                      model = "gemini-2.0-flash") {

  if (nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }

  model_query <- paste0(model, ":generateContent")

  request_times <- numeric(0)

  responses <- character(length(text_inputs))

  for (idx in seq_along(text_inputs)) {
    text_input <- text_inputs[idx]
    full_prompt <- paste0(
      "TASK: Analyze the sentiment of the given text and classify it as **Positive, Neutral, or Negative**.

      INSTRUCTIONS:
      - Identify the overall sentiment:
        - **Positive**: The text expresses enjoyment, excitement, satisfaction, or appreciation.
        - **Neutral**: The text states facts without emotion or shows a mix of positive and negative opinions.
        - **Negative**: The text expresses frustration, dissatisfaction, or disappointment.
      - If a statement lists **favorite things** (e.g., My favorite foods are...), assume **Positive** sentiment unless negative language is present.
      - The output should be **only the sentiment label**: `Positive`, `Neutral`, or `Negative`.

      EXAMPLES:

      1. **Input:** I love pizza, sushi, and burgers!
         **Output:** Positive

      2. **Input:** My top 3 movies are Inception, The Matrix, and Interstellar.
         **Output:** Positive (Because it lists `top` choices)

      3. **Input:** Movies I watched last month: Inception, Titanic, The Dark Knight.
         **Output:** Neutral (Just listing without opinion)

      4. **Input:** I hate horror movies. They're terrible.
         **Output:** Negative

      ===== INPUT TEXT BELOW =====
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

    if(response$status_code>200) {
      stop(paste("Error - ", content(response)$error$message))
    }

    candidates <- content(response)$candidates
    outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))

    return(gsub("\n$", "", outputs))

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

  return(responses)
}

# text_input <- "im getting on borderlands and i will murder you all ,"
# cat("im getting on borderlands and i will murder you all\n➡️", gemini_sentiment_analysis(text_input_1))

# text_input_2 <- "I went to work."
# cat("\nI went to work.\n➡️", gemini_sentiment_analysis(text_input_2))

# text_input_3 <- "My 4 fave games are Minecraft. Borderlands 2.Forza horizon 4. Lego star wars"
# cat("\nMy 4 fave games are Minecraft. Borderlands 2.Forza horizon 4. Lego star wars\n➡️", gemini_sentiment_analysis(text_input_3))
