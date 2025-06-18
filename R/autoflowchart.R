.dot_subgraphs <- function(texts, prefix, edge_style) {
  output <- c()
  step_list <- c()
  for (inode in seq_along(texts)) {
    step_name <- sprintf("%s%d", prefix, inode)
    step_list <- c(step_list, step_name)
    node_text <- texts[inode]
    output <- c(
      output,
      sprintf("%s[label=\"%s\"]", step_name, node_text)
    )
  }
  links <- paste(step_list, collapse = " -> ")
  output <- c(
    output,
    sprintf("subgraph notcluster%s {", prefix),
    "style=\"invis\";",
    "rank=same;",
    sprintf("edge [style=\"%s\"]", edge_style),
    paste0(links, ";"),
    "}"
  )
  return(output)
}

.dot_vertical_alignment <- function(sides_texts, nodes_prefix, sides_prefix) {
  output <- c()
  for (iside in seq_along(sides_texts)) {
    node_name <- sprintf("%s%d", nodes_prefix, iside)
    side_name <- sprintf("%s%d", sides_prefix, iside)
    output <- c(
      output,
      paste(c(node_name, side_name), collapse = " -> "),
      ";"
    )
  }
  return(output)
}

.dot_string <- function(nodes_texts, sides_texts) {
  #
  output <- c(
    "digraph flowchart {",
    "rankdir=LR;",
    "nodesep=0.5;",
    "ranksep=1.;",
    "splines=ortho;",
    "node [shape=box];"
  )
  #
  output <- c(output, .dot_subgraphs(nodes_texts, "STEP", edge_style = "solid"))
  output <- c(output, .dot_subgraphs(sides_texts, "SIDE", edge_style = "invis"))
  # for vertical alignement
  output <- c(output, .dot_vertical_alignment(sides_texts, "STEP", "SIDE"))
  #
  output <- c(output, "}")
  return(paste(output, collapse = "\n"))
}

.execute_dot_string <- function(dot_string, svg_file) {
  tmp_dot <- tempfile(fileext = ".dot")
  writeLines(dot_string, tmp_dot)
  command <- sprintf("dot -Tsvg %s -o %s", tmp_dot, svg_file)
  system(command)
  return()
}


make_flowchart <- function(
  list_df,
  list_summary_func,
  list_comparison_func,
  subject = NULL,
  time = NULL,
  output_svg_file
) {
  stopifnot(is.list(list_df))
  stopifnot(all(sapply(list_df, is.data.frame)))
  #
  stopifnot(is.list(list_summary_func))
  stopifnot(length(list_df) == length(list_summary_func))
  stopifnot(all(sapply(list_summary_func, is.function)))
  #
  stopifnot(is.list(list_comparison_func))
  stopifnot((length(list_df) - 1) == length(list_comparison_func))
  stopifnot(all(sapply(list_comparison_func, is.function)))
  #
  stopifnot(is.null(subject) | subject %in% names(list_df[[1]]))
  stopifnot(is.null(time) | time %in% names(list_df[[1]]))
  stopifnot(is.character(output_svg_file))
  #
  nodes_texts <- c()
  for (i in seq_along(list_summary_func)) {
    summary_func <- list_summary_func[[i]]
    # storing df into environment to use them with .filter_args_and_call
    df <- list_df[[i]]
    summary_ <- .filter_args_and_call(summary_func, environment())
    nodes_texts <- c(nodes_texts, summary_)
  }
  #
  edges_texts <- c()
  for (i in seq_along(list_comparison_func)) {
    comp_func <- list_comparison_func[[i]]
    # storing df1 and df2 into environment to use them with .filter_args_and_call
    df1 <- list_df[[i]]
    df2 <- list_df[[i + 1]]
    comp <- .filter_args_and_call(comp_func, environment())
    edges_texts <- c(edges_texts, comp)
  }
  #
  dot_string <- .dot_string(nodes_texts, edges_texts)
  .execute_dot_string(dot_string, output_svg_file)
  return()
}
