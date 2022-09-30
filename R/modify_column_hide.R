#' Modify Hidden Columns
#'
#' \lifecycle{maturing}
#' Use these functions to hide or unhide columns in a gtsummary table.
#'
#' @inheritParams modify_table_styling
#'
#' @name modify_column_hide
#' @family Advanced modifiers
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' # hide 95% CI, and replace with standard error
#' modify_column_hide_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_column_hide(columns = ci) %>%
#'   modify_column_unhide(columns = std.error)
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "modify_column_hide_ex1.png", width = "45")`
#' }}
NULL

#' @rdname modify_column_hide
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @export
modify_column_hide <- function(x, columns) {
  .assert_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_column_hide = match.call()))
  x <-
    modify_table_styling(
      x = x,
      columns = {{ columns }},
      hide = TRUE
    )

  x$call_list <- updated_call_list
  x
}

#' @rdname modify_column_hide
#' @export
modify_column_unhide <- function(x, columns) {
  .assert_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_column_unhide = match.call()))
  x <-
    modify_table_styling(
      x = x,
      columns = {{ columns }},
      hide = FALSE
    )

  x$call_list <- updated_call_list
  x
}
