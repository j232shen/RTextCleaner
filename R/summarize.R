library(httr)
library(jsonlite)

prompt <- "TASK: Please summarize the following technical text into 4-5 sentences. Focus on rephrasing the main points in a clear and concise manner. Avoid jargon, or rephrase any complex technical terms into simple, layman-friendly language so that the summary is easily digestible for someone without expertise in the field. The tone should be neutral and objective, and the summary should cover the most important insights from the text, ensuring that the essence of the content is preserved without overwhelming details.

STRICT RULES:
- Return ONLY the summarized text.
- Do not include any extra commentary or add unrelated content.

===== INPUT BELOW ====="

# Initialize deque to track request times
assign("request_times", numeric(0), envir = .GlobalEnv)

#' Summarize Technical Text using Gemini API
#'
#' This function summarizes a column of text inputs using the Gemini API.
#' It ensures rate limiting compliance and formats the responses cleanly.
#'
#' @param text_inputs A character vector containing text to be summarized.
#' @param temperature A numeric value controlling response randomness (default: 1).
#' @param max_output_tokens Maximum number of tokens in the response (default: 1024).
#' @param api_key A character string for the API key (default: retrieved from environment variable `GEMINI_API_KEY`).
#' @param model The Gemini model version to use (default: "gemini-2.0-flash").
#'
#' @return A character vector of the same length as `text_inputs`, containing the summarized text.
#' @export
#'
#' @examples
#' \dontrun{
#' text_samples <- c("A complex technical paper discussing AI advancements.",
#'                   "Research on quantum computing breakthroughs.")
#' summarize(text_samples)
#' }

summarize <- function(text_inputs,
                      temperature=1,
                      max_output_tokens=1024,
                      api_key=Sys.getenv("GEMINI_API_KEY"),
                      model = "gemini-2.0-flash") {

  # =================================== COPY ===================================
  # check for empty input and input type
  check_valid_inputs(text_inputs)

  # check for api key in environment; prompt for key if none exists
  check_api_key(api_key)
  # ============================================================================

  model_query <- paste0(model, ":generateContent")

  # create an empty character vector of the same length as input
  responses <- character(length(text_inputs))

  # loop through each text input
  for(idx in seq_along(text_inputs)) {
    text_input <- text_inputs[idx]
    full_prompt <- paste(prompt, text_input)

    print(paste("Processing", idx, "of", length(text_inputs)))

    # ensure we don't exceed 15 requests per minute
    current_time <- Sys.time()
    if(length(request_times) == 15) {
      time_since_first_request <- as.numeric(difftime(current_time, request_times[1], units = "secs"))
      if(time_since_first_request < 60) {
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

    # ================================== COPY ==================================
    # check for response error
    check_response_status(response)
    # ==========================================================================

    # extract candidates
    candidates <- content(response)$candidates

    # loop through each candidate and extract the text content
    for(candidate in candidates) {
      response_text <- gsub("\n", "", candidate$content$parts[[1]]$text) # remove newlines
      responses[idx] <- response_text
    }

    # record the timestamp of the request
    request_times <<- c(request_times, current_time)
    if(length(request_times) > 15) {
      request_times <<- request_times[-1]
    }
  }

  return(responses)
}
