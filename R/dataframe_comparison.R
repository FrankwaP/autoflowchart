# combining functions ----
combine_comparaisons <- function(...) {
  func <- function(df1, df2, subject = NULL, time = NULL) {
    output <- c()
    for (f in list(...)) {
      stopifnot(is.function(f))
      stopifnot(all(formalArgs(f) %in% c("df1", "df2", "subject", "time")))
      out <- .filter_args_and_call(f, environment())
      output <- c(output, out)
    }
    return(paste(output, collapse = "\n"))
  }
  return(func)
}


# variables modifications ----
list_removed_columns <- function(df1, df2) {
  diff <- setdiff(names(df1), names(df2))
  if (length(diff)) {
    return(sprintf(
      "%d columns removed:\n%s\n",
      length(diff),
      .print_character_vector(diff)
    ))
  }
}

# subject modifications ----
list_removed_subjects <- function(df1, df2, subject) {
  stopifnot(subject %in% names(df1))
  stopifnot(subject %in% names(df2))
  diff <- setdiff(df1[, subject], df2[, subject])
  if (length(diff)) {
    return(sprintf(
      "%d subjects removed:\n%s\n",
      length(diff),
      .print_character_vector(diff)
    ))
  }
}


# observations modifications ----
list_removed_observations <- function(df1, df2) {
  diff <- setdiff(rownames(df1), rownames(df2))
  if (length(diff)) {
    return(sprintf(
      "%d observations removed\n",
      length(diff)
    ))
  }
}


count_na_removed_observations <- function(df1, df2) {
  rem_df <- df1[setdiff(rownames(df1), rownames(df2)), ]
  out <- colSums(is.na(rem_df))
  out <- out[out != 0]
  return(sprintf(
    "NA values in the removed observations: \n%s",
    .print_numeric_vector(out)
  ))
}

# values modifications ----

.which_modif_vals_as_na <- function(df1, df2) {
  stopifnot(all(colnames(df2) %in% colnames(df1)))
  stopifnot(all(rownames(df2) %in% rownames(df1)))
  df_diff <- df1
  df_diff[,] <- FALSE # nolint
  # values changed
  rn <- rownames(df2)
  cn <- colnames(df2)
  df_diff[rn, cn] <- (!is.na(df1[rn, cn])) & is.na(df2)
  stopifnot(sum(is.na(df_diff)) == 0)
  return(df_diff)
}

.which_modif_vals_as_vals <- function(df1, df2) {
  stopifnot(all(colnames(df2) %in% colnames(df1)))
  stopifnot(all(rownames(df2) %in% rownames(df1)))
  df_diff <- df1
  df_diff[,] <- FALSE # nolint
  # values changed
  rn <- rownames(df2)
  cn <- colnames(df2)
  df_diff[rn, cn] <- (!(is.na(df1[rn, cn]) | is.na(df2))) & (df1[rn, cn] != df2)
  stopifnot(sum(is.na(df_diff[rn, cn]) == 0))
  return(df_diff)
}


list_outliers_as_na <- function(df1, df2) {
  df_diff <- .which_modif_vals_as_na(df1, df2)
  sum_diff <- colSums(df_diff)
  col_diff <- names(sum_diff[sum_diff != 0])
  if (length(col_diff)) {
    output <- c("Ouliers changed as NA:")
    for (col in col_diff) {
      changed <- df1[, col][df_diff[, col]]
      if (length(changed)) {
        # outliers can be NAed because too small or too big
        chg_inf <- changed[changed < min(df2[, col], na.rm = TRUE)]
        chg_sup <- changed[changed > max(df2[, col], na.rm = TRUE)]
        txt_inf <- .shorten_num_vector(chg_inf)
        txt_sup <- .shorten_num_vector(chg_sup)
        str_changed <- paste(c(txt_inf, txt_sup), collapse = ", ")
        output <- c(output, sprintf("\t%s: %s", col, str_changed))
      }
    }
    return(paste(output, collapse = "\n"))
  }
}

# stats about modifications
count_outliers_as_na <- function(df1, df2) {
  df_diff <- .which_modif_vals_as_na(df1, df2)
  out <- colSums(df_diff)
  out <- out[out != 0]
  return(sprintf("NA values: \n%s", .print_numeric_vector(out)))
}
