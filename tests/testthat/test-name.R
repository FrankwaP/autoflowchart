# TEST----
# df1 <- mtcars
# df2 <- df1
# df2[df2[, 'mpg'] > 20, "mpg"] <- NA
# cat(count_values_as_na(df1, df2), '\n')
# cat(list_values_as_na(df1, df2), '\n')
#
# df3 <- df2[complete.cases(df2), ]
#
# cat(compare_observations(df2, df3), '\n')

test_that("multiplication works", {
  data0 <- lcmm::data_hlme

  # outliers
  data1 <- data0
  data1[(data1$X3 < 0) | (data1$X3 > 3), 'X3'] <- NA
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
    Ni <- length(unique(df$ID))
    Nr <- nrow(df)
    Nv <- sum(!is.na(df))
    output <- sprintf("Nind=%d\nNobs=%d\nNval=%d", Ni, Nr, Nv)
    return(output)
  }

  make_flowchart(
    list_df = list(data0, data1, data2),
    list_summary_func = list(summarize, summarize, summarize),
    list_comparison_func = list(task1, task2),
    subject = 'ID',
    time = 'Time',
    output_svg_file = "test-flowchart.svg"
  )
})
