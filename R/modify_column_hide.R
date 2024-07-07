#' Modify hidden columns
#'
#' Use these functions to hide or unhide columns in a gtsummary table.
#' Use `show_header_names(show_hidden=TRUE)` to print available columns to update.
#'
#' @inheritParams modify_table_styling
#'
#' @name modify_column_hide
#' @author Daniel D. Sjoberg
#'
#' @examples
#' # Example 1 ----------------------------------
#' # hide 95% CI, and replace with standard error
#' lm(age ~ marker + grade, trial) |>
#'   tbl_regression() |>
#'   modify_column_hide(conf.low) |>
#'   modify_column_unhide(columns = std.error)
NULL

#' @rdname modify_column_hide
#' @export
modify_column_hide <- function(x, columns) {
  set_cli_abort_call()
  check_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_column_hide = match.call()))

  # hide columns ---------------------------------------------------------------
  x <-
    modify_table_styling(
      x = x,
      columns = {{ columns }},
      hide = TRUE
    )

  # return updated object ------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @rdname modify_column_hide
#' @export
modify_column_unhide <- function(x, columns) {
  set_cli_abort_call()
  check_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_column_unhide = match.call()))

  # unhide columns -------------------------------------------------------------
  x <-
    modify_table_styling(
      x = x,
      columns = {{ columns }},
      hide = FALSE
    )

  # return updated object ------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
