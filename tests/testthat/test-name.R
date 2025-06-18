test_that("basic workflow", {
  data0 <- ChickWeight

  # outliers
  data1 <- data0
  idx <- (data1[["weight"]] < 50) | (data1[["weight"]] > 300)
  data1[idx, "weight"] <- NA
  task1 <- list_outliers_as_na

  # remove incomplete observations
  data2 <- data1
  data2 <- data2[complete.cases(data2), ]
  task2 <- combine_func(
    list_removed_observations,
    list_removed_subjects,
    count_na_removed_observations
  )

  summarize <- function(df) {
    n_ind <- length(unique(df$ID))
    n_row <- nrow(df)
    n_val <- sum(!is.na(df))
    output <- sprintf("Nind=%d\nNobs=%d\nNval=%d", n_ind, n_row, n_val)
    return(output)
  }

  make_flowchart(
    list_df = list(data0, data1, data2),
    list_summary_func = list(summarize, summarize, summarize),
    list_comparison_func = list(task1, task2),
    subject = "Chick",
    time = "Time",
    output_svg_file = "test-flowchart.svg"
  )
})
