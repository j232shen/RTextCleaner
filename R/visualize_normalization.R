#' Tokenize text into individual words
#'
#' Breaks down text into individual tokens (words) for frequency analysis.
#' Converts to lowercase, removes punctuation, numbers, and common stopwords.
#'
#' @param text A character vector containing text to tokenize
#' @param language Language for stopwords (default: "en")
#'
#' @return A character vector of tokens
#' @keywords internal
tokenize_text <- function(text, language = "en") {

  # Handle NULL or empty input
  if(is.null(text) || length(text) == 0) {
    return(character(0))
  }

  # Check for stopwords package availability
  if(!requireNamespace("stopwords", quietly = TRUE)) {
    stop("Package 'stopwords' is required. Please install it with install.packages('stopwords')")
  }

  # Process each element in the vector
  all_tokens <- c()

  for(t in text) {
    if(is.na(t) || t == "") next

    # process text
    t <- tolower(t) # convert to lowercase
    t <- gsub("[[:punct:]]+", " ", t) # remove punctuation
    t <- gsub("\\b\\d+\\b", "", t) # remove numbers

    # split text into tokens
    element_tokens <- unlist(strsplit(t, "\\s+"))

    # remove empty tokens
    element_tokens <- element_tokens[element_tokens != ""]

    # add to collection
    all_tokens <- c(all_tokens, element_tokens)
  }

  # remove stopwords
  stopword_list <- stopwords::stopwords(language)
  all_tokens <- all_tokens[!all_tokens %in% stopword_list]

  return(all_tokens)
}
