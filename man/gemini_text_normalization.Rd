% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization.R
\name{gemini_text_normalization}
\alias{gemini_text_normalization}
\title{Normalize Text Using Gemini API}
\usage{
gemini_text_normalization(
  text_inputs,
  temperature = 1,
  max_output_tokens = 1024,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  model = "gemini-2.0-flash"
)
}
\arguments{
\item{text_inputs}{A character vector containing text to be normalized.}

\item{temperature}{A numeric value controlling response randomness (default: 1).}

\item{max_output_tokens}{Maximum number of tokens in the response (default: 1024).}

\item{api_key}{A character string for the API key (default: retrieved from environment variable \code{GEMINI_API_KEY}).}

\item{model}{The Gemini model version to use (default: "gemini-2.0-flash").}
}
\value{
A character vector of the same length as \code{text_inputs}, containing the normalized text.
}
\description{
This function normalizes a column of text inputs by correcting capitalization, fixing spelling errors,
and expanding informal abbreviations while maintaining readability and meaning.
}
\examples{
\dontrun{
  text_samples <- c("omg dis is da best day evr!!!", "yayyy, tysm this is sooo gr8!!!")
  normalized_text <- gemini_text_normalization(text_samples)
  print(normalized_text)
}
}
