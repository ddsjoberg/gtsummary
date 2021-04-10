#' Modify Hidden Columns
#'
#' \lifecycle{experimental}
#' Use these functions to hide or unhide columns in a gtsummary tables.
#'
#' @inheritParams modify_table_styling
#'
#' @name modify_column_hide
#' @family Advanced modifiers
#' @examples
#' # Example 1 ----------------------------------
#' # hide 95% CI, and replace with standard error
#' modify_column_hide_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_column_hide(columns = ci) %>%
#'   modify_column_unhide(columns = std.error)
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_column_hide_ex1.png}{options: width=45\%}}
NULL

#' @rdname modify_column_hide
#' @export
modify_column_hide <- function(x, columns) {
  modify_table_styling(
    x = x,
    columns = {{ columns }},
    hide = TRUE
  )
}

#' @rdname modify_column_hide
#' @export
modify_column_unhide <- function(x, columns) {
  modify_table_styling(
    x = x,
    columns = {{ columns }},
    hide = FALSE
  )
}
