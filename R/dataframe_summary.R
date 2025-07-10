# combining functions ----
combine_summaries <- function(...) {
  func <- function(df, subject, time = NULL) {
    output <- c()
    for (func in list(...)) {
      out <- .filter_args_and_call(
        func,
        df = df,
        subject = subject,
        time = time
      )
      output <- c(output, out)
    }
    return(paste(output, collapse = "\n"))
  }
  return(func)
}

# function…
summarize_nb_subjects <- function(df, subject) {
  stopifnot(subject %in% names(df))
  n_ind <- length(unique(df[[subject]]))
  return(sprintf("Nind=%d", n_ind))
}


summarize_nb_observations <- function(df) {
  n_row <- nrow(df)
  df_ <- df[complete.cases(df), ]
  n_row_ <- nrow(df_)
  return(sprintf("Nobs=%d (%d complete)", n_row, n_row_))
}
