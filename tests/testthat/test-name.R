test_that("basic workflow", {
  data0 <- ChickWeight

  # outliers
  data1 <- data0
  idx <- (data1[["weight"]] < 50) | (data1[["weight"]] > 300)
  data1[idx, "weight"] <- NA
  task1 <- compare_after_outliers_removal

  # remove incomplete observations
  data2 <- data1
  data2 <- data2[complete.cases(data2), ]
  task2 <- combine_comparaisons(
    compare_observations,
    compare_subjects,
    compare_columns_with_na
  )

  summarize <- combine_summaries(
    summarize_nb_subjects,
    summarize_nb_observations
  )

  make_flowchart(
    list_df = list(data0, data1, data2),
    list_summary_func = list(summarize, summarize, summarize),
    list_comparison_func = list(task1, task2),
    list_comparison_title = list("TASK1", "TASK2"),
    subject = "Chick",
    time = "Time",
    output_svg_file = "test-flowchart.svg"
  )
  file.remove("test-flowchart.svg")
})
