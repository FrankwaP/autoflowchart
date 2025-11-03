
<!-- README.md is generated from README.Rmd. Please edit that file -->

`autoflowchart` is a R package to help you build flowcharts for data
management or cohort follow up.

To use it, download this repo then use: - install it using
`devtools::install_github("https://github.com/FrankwaP/autoflowchart")` -
or clone this repo and use `devtool::load_all("LOCAL_PATH_TO_THE_REPO")`
(easier to debug or add modifications).

It writes down a [Graphviz](https://graphviz.org/) `.dot` file and calls
Graphviz. So Graphviz must be installed and the `dot` command must
exist in your system’s `PATH`.

It considers:

- the main branch, for which summaries of the datasets at
each step are displayed,  
- the side nodes, for which comparisons between
the old and new datasets are displayed.

The summaries are obtained using functions which: - return a string or
`NULL` - uses specific arguments:

- for the datasets summaries: `df` and optionally `subject` and `time`  
- for the modifications summaries: `df1`, `df2` and
- optionally `subject` and `time`  

Some functions are already coded, but you can defined yours as long as
they respect these constraints.

Here is a basic example:

``` r
# example of data-management ----

## original dataset ----
data0 <- ChickWeight

#
title1 <- "remove outliers"
task1 <- compare_after_outliers_removal
data1 <- data0
idx <- (data1[["weight"]] < 50) | (data1[["weight"]] > 300)
data1[idx, "weight"] <- NA
#
title2 <- "remove incomplete observations"
task2 <- combine_comparaisons(
  compare_incomplete_observations,
  compare_subjects,
  compare_columns_with_na
)
data2 <- data1
data2 <- data2[complete.cases(data2), ]


# function to summarize the information of a dataset
summarize <- combine_summaries(summarize_nb_subjects, summarize_nb_observations)

# Generating the flowchart ----
make_flowchart(
  list_df = list(data0, data1, data2),
  list_summary_func = list(summarize, summarize, summarize),
  list_comparison_func = list(task1, task2),
  list_comparison_title = list(title1, title2),
  subject = "Chick",
  time = "Time",
  output_svg_file = "test-flowchart.svg"
)
knitr::include_graphics("test-flowchart.svg")
```

<img src="vignettes/test-flowchart.svg" width="100%" />
