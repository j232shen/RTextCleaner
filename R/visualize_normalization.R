#' Tokenize Text into Individual Words
#'
#' Breaks down text into individual tokens (words) for frequency analysis.
#' Converts to lowercase, removes punctuation, numbers, and common stopwords.
#'
#' @param text_inputs A character vector containing text to tokenize
#'
#' @return A character vector of tokens
#' @keywords internal
#' @name tokenize_text
#' @title Tokenize Text into Individual Words
#' @description Breaks down text into individual tokens (words) for frequency analysis. Converts to lowercase, removes punctuation, numbers, and common stopwords.

tokenize_text <- function(text_inputs) {
  # handle NULL or empty input
  if(is.null(text_inputs) || length(text_inputs) == 0) {
    stop("Input is empty or NULL.")
  }

  # process each element in the vector
  all_tokens <- c()

  for(t in text_inputs) {
    # handle encoding issues
    t <- iconv(t, from = "UTF-8", to = "UTF-8", sub = "")

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

  return(all_tokens)
}


#' Visualize the effects of text normalization
#'
#' Creates a visualization highlighting the tokens that changed most significantly
#' after normalization, focusing on spelling corrections and informal language conversion.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom utils adist
#' @importFrom stats reorder
#'
#' @param top_n Number of most significant changes to display (default: 20)
#' @param original_text Original text vector (optional if normalization was run)
#' @param normalized_text Normalized text vector (optional if normalization was run)
#'
#' @return A ggplot object showing normalization effects
#' @export
visualize_normalization <- function(top_n = 20, original_text = NULL, normalized_text = NULL) {

  # if text vectors aren't explicitly provided, use the last normalization result
  if(is.null(original_text) || is.null(normalized_text)) {
    # get from environment variable where the last normalization was stored
    last_norm <- get_last_normalization()

    if(is.null(last_norm)) {
      stop("No normalization data found. Please run normalization first or provide text vectors.")
    }

    original_text <- last_norm$original
    normalized_text <- last_norm$normalized
  }

  if (length(original_text) == 0 || length(normalized_text) == 0) {
    stop("Original and normalized text vectors must have the same length.")
  }

  if (length(original_text) != length(normalized_text)) {
    stop("Original and normalized text vectors must have the same length.")
  }

  # check if both are character vectors
  if (!is.character(original_text)) {
    stop("Original text must be a character vector.")
  }

  if (!is.character(normalized_text)) {
    stop("Normalized text must be a character vector.")
  }

  # create a data frame of the original and normalized texts
  text_df <- data.frame(
    original = original_text,
    normalized = normalized_text,
    stringsAsFactors = FALSE
  )

  # find instances where the text changed
  text_df$changed <- text_df$original != text_df$normalized

  # calculate what percentage of texts were modified
  pct_changed <- sum(text_df$changed) / nrow(text_df) * 100

  # find examples of transformations
  changed_df <- text_df[text_df$changed, ]

  # exit early if no changes
  if (nrow(changed_df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0, label = "No significant text changes found") +
             ggplot2::theme_void())
  }

  # extract word-level changes
  changes <- list()

  for(i in 1:min(nrow(changed_df), 100)) {  # Process up to 100 changed texts
    orig <- changed_df$original[i]
    norm <- changed_df$normalized[i]

    # tokenize both versions
    orig_tokens <- tokenize_text(orig)
    norm_tokens <- tokenize_text(norm)

    # find tokens that exist in only one version
    only_in_orig <- setdiff(orig_tokens, norm_tokens)
    only_in_norm <- setdiff(norm_tokens, orig_tokens)

    # add to collection of changes
    for(old_token in only_in_orig) {
      # find best match in normalized tokens
      if(length(only_in_norm) > 0) {
        distances <- adist(old_token, only_in_norm)
        best_match <- only_in_norm[which.min(distances)]

        # only record if reasonably close
        if(min(distances) <= nchar(old_token) * 0.5) {
          changes[[length(changes) + 1]] <- data.frame(
            original = old_token,
            normalized = best_match,
            distance = min(distances),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  # combine all changes
  all_changes <- do.call(rbind, changes)

  # count how many times each transformation occurred
  changes_table <- table(paste(all_changes$original, "->", all_changes$normalized))
  changes_df <- data.frame(
    transformation = names(changes_table),
    count = as.numeric(changes_table)
  )

  # extract original and normalized components
  changes_df$original <- sapply(strsplit(changes_df$transformation, " -> "), `[`, 1)
  changes_df$normalized <- sapply(strsplit(changes_df$transformation, " -> "), `[`, 2)

  # get top transformations
  top_changes <- changes_df[order(-changes_df$count), ][1:min(top_n, nrow(changes_df)), ]

  # create transformation frequency plot
  plt <- ggplot2::ggplot(top_changes, ggplot2::aes(x = reorder(transformation, count), y = count)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Top Text Transformations",
      subtitle = paste0(round(pct_changed, 1), "% of texts were modified"),
      x = "Transformation",
      y = "Frequency"
    )

  return(plt)
}
