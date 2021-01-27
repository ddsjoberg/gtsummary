#' Modify Hidden Columns
#'
#' \lifecycle{experimental}
#' Use these functions to hide or unhide columns in a gtsummary tables.
#'
#' @inheritParams modify_table_header
#'
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' # hide 95% CI, and replace with standard error
#' modify_column_hide_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_column_hide(column = ci) %>%
#'   modify_column_unhide(column = std.error)
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_column_hide_ex1.png}{options: width=45\%}}
modify_column_hide <- function(x, column) {
  modify_table_header(
    x = x,
    column = {{ column }},
    hide = TRUE
  )
}

#' @name modify_column_hide
#' @export
modify_column_unhide <- function(x, column) {
  modify_table_header(
    x = x,
    column = {{ column }},
    hide = FALSE
  )
}
