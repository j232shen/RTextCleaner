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
#' \dontrun{
#'   text_samples <- c("I love pizza, sushi, and burgers!", "I hate horror movies. They're terrible.")
#'   sentiment_results <- gemini_sentiment_analysis(text_samples)
#'   print(sentiment_results)
#' }

gemini_sentiment_analysis <- function(text_inputs,
                                      temperature = 1,
                                      max_output_tokens = 1024,
                                      api_key = Sys.getenv("GEMINI_API_KEY"),
                                      model = "gemini-2.0-flash") {

  # check for empty input and input type
  check_valid_inputs(text_inputs)

  # check for api key in environment; prompt for key if none exists
  check_api_key(api_key)

  model_query <- paste0(model, ":generateContent")

  responses <- character(length(text_inputs))

  for (idx in seq_along(text_inputs)) {
    text_input <- text_inputs[idx]

    # Define prompt
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

    # ensure we don't exceed 15 requests per minute
    current_time <- Sys.time()
    if(length(rate_limit_env$request_times) == 15) {
      time_since_first_request <- as.numeric(difftime(current_time, rate_limit_env$request_times[1], units = "secs"))

      if(time_since_first_request < 60) {
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

    # check for response error
    check_response_status(response)

    # Extract response
    response_content <- httr::content(response)
    if (is.null(response_content$candidates) || length(response_content$candidates) == 0 ||
        is.null(response_content$candidates[[1]]$content$parts[[1]]$text)) {
      stop("Error: Unexpected response format from API")
    }

    # Store cleaned text
    responses[idx] <- gsub("\n", "", response_content$candidates[[1]]$content$parts[[1]]$text)

    # Record request time
    rate_limit_env$request_times <- c(rate_limit_env$request_times, current_time)
    if (length(rate_limit_env$request_times) > 15) {
      rate_limit_env$request_times <- rate_limit_env$request_times[-1]
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
