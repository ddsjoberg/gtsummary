#' Add/Remove Indentation
#'
#'
#'
#' @param x a gtsummary table
#' @param columns columns to add indentation to
#' @param rows predicate expression to select rows to indent. Default is `NULL`,
#' indicating all rows
#' @param double_indent logical indicating whether to double indent the cells.
#' Default is `FALSE` for a single indentation
#' @param undo logical indicating whether an indentation should be removed/undone.
#' Default is `FALSE`
#'
#' @return a gtsummary table
#' @export
#'
#' @family Advanced modifiers
#' @examples
#' # remove indentation from `tbl_summary()`
#' modify_column_indent_ex1 <-
#'   trial %>%
#'   select(grade) %>%
#'   tbl_summary() %>%
#'   modify_column_indent(columns = label, undo = TRUE)
modify_column_indent <- function(x, columns, rows = NULL,
                                 double_indent = FALSE, undo = FALSE) {
  modify_table_styling(
    x,
    columns = {{ columns }},
    rows = {{ rows }},
    text_format = ifelse(double_indent, "indent2", "indent"),
    undo_text_format = undo
  )
}
