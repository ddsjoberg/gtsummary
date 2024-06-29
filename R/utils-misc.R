.extract_glue_elements <- function(x) {
  regmatches(x, gregexpr("\\{([^\\}]*)\\}", x)) |>
    unlist() %>%
    {
      substr(., 2, nchar(.) - 1)
    }
}

.ifelse1 <- function(test, yes, no) {
  if (test) {
    return(yes)
  }
  no
}

case_switch <- function(..., .default = NULL) {
  dots <- dots_list(...)

  for (f in dots) {
    if (isTRUE(eval(f_lhs(f), envir = attr(f, ".Environment")))) {
      return(eval(f_rhs(f), envir = attr(f, ".Environment")))
    }
  }

  return(.default)
}

vec_paste0 <- function(..., collapse = NULL) {
  args <- vctrs::vec_recycle_common(...)
  rlang::inject(paste0(!!!args, collapse = collapse))
}

# used in the as_flex_table (and friends) functions for inserting calls
add_expr_after <- function(calls, add_after, expr, new_name = NULL) {
  # checking input
  if (!rlang::is_string(add_after) || !add_after %in% names(calls)) {
    cli::cli_abort("Argument {.arg add_after} must be one of {.val {names(calls)}}.")
  }

  # position to insert, and name of list
  index <- which(names(calls) == add_after)
  new_name <- new_name %||% "user_added"
  new_list <- list(expr) %>% set_names(new_name)

  # insert list
  append(calls, new_list, after = index)
}


#' Is a date/time
#'
#' `is_date_time()`: Predicate for date, time, or date-time vector identification.
#'
#' @param x a vector
#'
#' @return a scalar logical
#' @keywords internal
#'
#' @examples
#' iris |>
#'   dplyr::mutate(date = as.Date("2000-01-01") + dplyr::row_number()) |>
#'   lapply(gtsummary:::is_date_time)
is_date_time <- function(x) {
  inherits(x, c("Date", "POSIXct", "POSIXlt"))
}
