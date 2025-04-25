# autoflowchart
R package to help you build flowcharts on R for data management or cohort follow up.

![](/tests/testthat/test-flowchart.svg)

To use it, download this repo then use: 
- install it using `devtools::install_github("https://github.com/FrankwaP/autoflowchart")`
- or clone this repo and use `devtool::load_all("LOCAL_PATH_TO_THE_REPO")` (easier to debug or add modifications).

It writes down a [Graphviz](https://graphviz.org/) .dot file and call Graphviz.  
So it must be installed and the `dot` command must exist in your system's `PATH`.

It considers:
 - the main branch, for which summaries of the datasets at each step are displayed
 - the side nodes, for which summaries of the modifications at each step are displayed
 
The summaries are obtained using functions which:
 - return a string or `NULL`
 - uses specific arguments:
  - for the datasets summaries: `df` and optionally `subject` and `time`
  - for the modifications summaries: `df1`, `df2` and optionally `subject` and `time`

Some functions are already coded, but you can defined yours as long as they respect these constraints.

Please see the (very basic) test file for an example.

