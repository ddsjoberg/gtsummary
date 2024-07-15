#' Modify formatting functions
#'
#' Use this function to update the way numeric columns and rows of `.$table_body`
#' are formatted
#'
#' @param ... [`dynamic-dots`][rlang::dyn-dots]\cr
#'   Used to assign updates to formatting functions.
#'
#'   Use `modify_fmt_fun(colname = <fmt fn>)` to update a single column. Using a
#'   formula will invoke tidyselect, e.g. `modify_fmt_fun(c(estimate, conf.low, conf.high) ~ <fmt_fun>)`.
#'
#'   Use the `show_header_names()` to see the column names that can be modified.
#' @inheritParams modify
#' @inheritParams modify_table_styling
#'
#' @inheritSection modify_table_styling rows argument
#'
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' # show 'grade' p-values to 3 decimal places and estimates to 4 sig figs
#' lm(age ~ marker + grade, trial) |>
#'   tbl_regression() %>%
#'   modify_fmt_fun(
#'     p.value = label_style_pvalue(digits = 3),
#'     c(estimate, conf.low, conf.high) ~ label_style_sigfig(digits = 4),
#'     rows = variable == "grade"
#'   )
modify_fmt_fun <- function(x, ..., rows = NULL, update, quiet) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_column_unhide = match.call()))
  check_class(x, "gtsummary")

  # process inputs -------------------------------------------------------------
  dots <- dots_list(...)
  dots <-
    .deprecate_modify_update_and_quiet_args(
      dots = dots, update = update, quiet = quiet, calling_fun = "modify_fmt_fun"
    )

  # converting dots arg to a tidyselect list -----------------------------------
  cards::process_formula_selectors(
    data = scope_header(x$table_body, x$table_styling$header),
    dots = dots
  )
  cards::check_list_elements(
    x = dots,
    predicate = \(x) is_function(x),
    error_msg = "The value passed in {.arg {arg_name}} for variable {.val {variable}} must be a function."
  )

  # updating formatting functions ----------------------------------------------
  x <-
    modify_table_styling(
      x = x,
      columns = names(dots),
      rows = {{ rows }},
      fmt_fun = dots
    )

  x$call_list <- updated_call_list
  x
}
