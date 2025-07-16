FAKE_PREFIX <- "FAKE"
SUMM_PREFIX <- "SUMMARY"
COMP_PREFIX <- "COMPARAISON"

.dot_subgraphs <- function(texts, prefix, edge_style, add_fake_steps) {
  stopifnot(prefix %in% c(SUMM_PREFIX, COMP_PREFIX))
  node_list <- c()
  step_list <- c()
  # nodes
  for (inode in seq_along(texts)) {
    step_name <- sprintf("%s%d", prefix, inode)
    step_list <- c(step_list, step_name)
    node_text <- texts[inode]
    node_list <- c(
      node_list,
      sprintf("%s[label=\"%s\"]", step_name, node_text)
    )
    if (add_fake_steps && inode != length(texts)) {
      fake_step_name <- sprintf("%s%d", FAKE_PREFIX, inode)
      step_list <- c(step_list, fake_step_name)
      node_list <- c(
        node_list,
        sprintf("%s[shape=point]", fake_step_name)
      )
    }
  }
  # links
  link_list <- c()
  for (istep in seq_along(step_list)[-length(step_list)]) {
    node1 <- step_list[[istep]]
    node2 <- step_list[[istep + 1]]
    if (startsWith(node2, FAKE_PREFIX)) {
      link_list <- c(
        link_list,
        sprintf("%s -> %s [arrowhead=none]", node1, node2)
      )
    } else {
      link_list <- c(link_list, sprintf("%s -> %s", node1, node2))
    }
  }
  # final
  output <- c(
    node_list,
    sprintf("subgraph notcluster%s {", prefix),
    "style=\"invis\";",
    "rank=same;",
    sprintf("edge [style=\"%s\"]", edge_style),
    link_list,
    "}"
  )
  return(output)
}

.dot_vertical_alignment <- function(sides_texts, nodes_prefix, sides_prefix) {
  output <- c()
  for (iside in seq_along(sides_texts)) {
    node_name <- sprintf("%s%d", FAKE_PREFIX, iside)
    side_name <- sprintf("%s%d", COMP_PREFIX, iside)
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
  output <- c(
    output,
    .dot_subgraphs(
      nodes_texts,
      SUMM_PREFIX,
      edge_style = "solid",
      add_fake_steps = TRUE
    )
  )
  output <- c(
    output,
    .dot_subgraphs(
      sides_texts,
      COMP_PREFIX,
      edge_style = "invis",
      add_fake_steps = FALSE
    )
  )
  # for vertical alignement
  output <- c(
    output,
    .dot_vertical_alignment(sides_texts, SUMM_PREFIX, COMP_PREFIX)
  )
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
  list_comparison_title,
  subject,
  time = NULL,
  output_svg_file
) {
  # NOTE: I trued using rasterImage and grid::grid.raster but it was
  #       too difficult rendering with good quality
  stopifnot(is.list(list_df))
  stopifnot(all(sapply(list_df, is.data.frame)))
  #
  stopifnot(is.list(list_summary_func))
  stopifnot(length(list_df) == length(list_summary_func))
  stopifnot(all(sapply(list_summary_func, is.function)))
  #
  stopifnot(is.list(list_comparison_func))
  stopifnot(all(sapply(list_comparison_func, is.function)))
  stopifnot((length(list_df) - 1) == length(list_comparison_func))
  #
  stopifnot(is.list(list_comparison_title))
  stopifnot(all(sapply(list_comparison_title, is.character)))
  stopifnot(length(list_comparison_func) == length(list_comparison_title))
  #
  stopifnot(is.null(subject) | subject %in% names(list_df[[1]]))
  stopifnot(is.null(time) | time %in% names(list_df[[1]]))
  stopifnot(is.character(output_svg_file))
  #
  nodes_texts <- c()
  for (i in seq_along(list_summary_func)) {
    summary_func <- list_summary_func[[i]]
    df <- list_df[[i]]
    summary_ <- .filter_args_and_call(
      summary_func,
      df = df,
      subject = subject,
      time = time
    )
    nodes_texts <- c(nodes_texts, summary_)
  }
  #
  edges_texts <- c()
  for (i in seq_along(list_comparison_func)) {
    comp_func <- list_comparison_func[[i]]
    df1 <- list_df[[i]]
    df2 <- list_df[[i + 1]]
    comp <- sprintf(
      "%s\n\n%s",
      list_comparison_title[[i]],
      .filter_args_and_call(
        comp_func,
        df1 = df1,
        df2 = df2,
        subject = subject,
        time = time
      )
    )
    edges_texts <- c(edges_texts, comp)
  }
  #
  dot_string <- .dot_string(nodes_texts, edges_texts)
  .execute_dot_string(dot_string, output_svg_file)
  return(invisible())
}
