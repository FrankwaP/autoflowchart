# utils ----
.wrapped_print <- function(...) {
  n_lines_max <- 2
  out <- paste(..., sep = ":", collapse = ", ")
  out <- strwrap(out, 40, exdent = 2)
  if (length(out) > n_lines_max) {
    out <- out[1:n_lines_max]
    out[n_lines_max] <- paste0(out[n_lines_max], " ...")
  }
  out <- paste(out, collapse = "\n")
  return(out)
}

.print_vector <- function(vec, str_code) {
  vecnames <- names(vec)
  vecstring <- sprintf(str_code, vec)
  if (is.null(vecnames)) {
    return(.wrapped_print(vecstring))
  }
  return(.wrapped_print(vecnames, vecstring))
}

.print_numeric_vector <- function(vec) {
  return(.print_vector(vec, "%.3g"))
}

.print_integer_vector <- function(vec) {
  return(.print_vector(vec, "%d"))
}

.print_character_vector <- function(vec) {
  return(.print_vector(vec, "%s"))
}


.shorten_num_vector <- function(vec) {
  vec <- sort(vec)
  lv <- length(vec)
  if (lv == 1) {
    return(.print_numeric_vector(vec[[1]]))
  } else if (lv > 1) {
    return(sprintf(
      "[%s, ..., %s]",
      .print_numeric_vector(vec[[1]]),
      .print_numeric_vector(vec[[lv]])
    ))
  }
}


.filter_args_and_call <- function(f, env) {
  # trick to handle function without subject and time argument
  env_list <- as.list(env)
  func_arg_names <- formalArgs(f)
  sdiff <- setdiff(func_arg_names, names(env_list))
  if (length(sdiff)) {
    stop(sprintf(
      "The following arguments names are not supposed to be used: %s",
      sdiff
    ))
  }
  stopifnot(all(func_arg_names %in% names(env_list)))
  out <- do.call(f, env_list[func_arg_names])
  stopifnot(is.character(out) | is.null(out))
  return(out)
}
