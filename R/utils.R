
check_valid_inputs <- function(text_inputs) {
  # check for empty input
  if (length(text_inputs) < 1) {
    stop("Error: text_inputs cannot be empty. Please provide at least one input.")
  }

  # check for incorrect input type
  if (!is.character(text_inputs)) {
    stop("Error: text_inputs must be a character vector.")
  }
}

check_response_status <- function(response) {
  if(response$status_code != 200) {
    error_message <- content(response)$error$message

    print(paste("Error - Status Code:", response$status_code))
    print(content(response))

    if (error_message == "API key not valid. Please pass a valid API key.") {
      Sys.unsetenv("GEMINI_API_KEY")
    }

    stop(paste("Error:", error_message))
  }
}

check_api_key <- function(api_key) {
  if(nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
}
