.extract_glue_elements <- function(x) {
  # this part removes double curlies pairs, trying to mimic how `glue::glue()` would find the element to evaluate
  while (isTRUE(any(str_detect(x, "\\{\\{.*\\}\\}")))) {
    x <- map_chr(x, ~ifelse(str_detect(x, "\\{\\{.*\\}\\}"), str_replace(.x, "(\\{\\{)|(\\}\\})", ""), .x))
  }

  # extract string from between the curlies
  regmatches(x, gregexpr("\\{([^\\}]*)\\}", x)) |>
    unlist() %>%
    substr(start = 2, stop = nchar(.) - 1)
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

# used in the as_flex_table (and friends) functions for inserting calls.
# when `add_after` is missing (NULL, NA, or ""), the expression is appended to the end
add_expr_after <- function(calls, add_after, expr, new_name = NULL) {
  # position to insert
  if (is.null(add_after) || isTRUE(is.na(add_after)) || identical(add_after, "")) {
    index <- length(calls)
  } else if (!rlang::is_string(add_after) || !add_after %in% names(calls)) {
    cli::cli_abort(
      c("Named elements in the {.code addl_cmds} theme elements must be one of {.val {names(calls)}}.",
        i = "Unnamed elements are appended to the end of the calls.")
    )
  } else {
    index <- which(names(calls) == add_after)
  }

  # name of list
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

.first_unhidden_column <- function(x) {
  x$table_styling$header |>
    dplyr::filter(!.data$hide) |>
    dplyr::pull("column") |>
    dplyr::first()
}
