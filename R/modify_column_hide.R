#' Modify Hidden Columns
#'
#' Use these functions to hide or unhide columns in a gtsummary tables.
#'
#' @inheritParams modify_table_header
#'
#' @export
#' @examples
#' modify_column_hide_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_column_hide(column = ci) %>%
#'   modify_column_unhide(column = std.error)
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
