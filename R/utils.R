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


.filter_args_and_call <- function(func, ...) {
  # handle functions with/without subject and time as argument
  stopifnot(is.function(func))
  cl <- match.call()
  params <- list(...)
  func_arg_names <- formalArgs(func)
  filtered_params <- params[func_arg_names]
  filtered_params <- filtered_params[!is.null(filtered_params)] # normally just 'time' can be filtered
  if (!setequal(func_arg_names, names(filtered_params))) {
    stop(sprintf(
      "The function '%s' has '%s' as arguments, but should use one of '%s'",
      cl$func,
      func_arg_names,
      params
    ))
  }
  out <- do.call(func, filtered_params)
  stopifnot(is.character(out) | is.null(out))
  return(out)
}
