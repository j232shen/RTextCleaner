# Internal state management for the package
# Not exported to users

.normalization_state <- new.env(parent = emptyenv())

store_normalization_result <- function(original, normalized) {
  .normalization_state$last_original <- original
  .normalization_state$last_normalized <- normalized
}

get_last_normalization <- function() {
  if(exists("last_original", envir = .normalization_state) &&
     exists("last_normalized", envir = .normalization_state)) {
    return(list(
      original = .normalization_state$last_original,
      normalized = .normalization_state$last_normalized
    ))
  } else {
    return(NULL)
  }
}
