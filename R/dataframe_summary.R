# combining functions ----
combine_summaries <- function(...) {
  func <- function(df, subject = NULL, time = NULL) {
    output <- c()
    for (f in list(...)) {
      stopifnot(is.function(f))
      stopifnot(all(formalArgs(f) %in% c("df", "subject", "time")))
      out <- .filter_args_and_call(f, environment())
      output <- c(output, out)
    }
    return(paste(output, collapse = "\n"))
  }
  return(func)
}

# function…
summarize_nb_subjects <- function(df, subject) {
  n_ind <- length(unique(df[[subject]]))
  return(sprintf("Nind=%d", n_ind))
}


summarize_nb_observations <- function(df) {
  n_row <- nrow(df)
  df_ <- df[complete.cases(df), ]
  n_row_ <- nrow(df_)
  return(sprintf("Nobs=%d (%d complete)", n_row, n_row_))
}
