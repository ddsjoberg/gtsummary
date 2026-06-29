#' gtsummary table dimension
#'
#' @description
#' Returns the dimension of a gtsummary table, that is, the number of rows
#' and the number of un-hidden columns.
#'
#' `nrow()` calls `dim()`; therefore, `nrow()` will also work on gtsummary tables.
#'
#' @param x (`gtsummary`)\cr
#'  a 'gtsummary' table
#'
#' @returns integer vector
#' @export
#' @keywords internal
#'
#' @examples
#' tbl <- tbl_summary(trial, include = age, by = trt)
#'
#' dim(tbl)
#' nrow(tbl)
dim.gtsummary <- function(x) {
  # select only unhidden columns
  columns <- x$table_styling$header[!x$table_styling$header$hide,]$column

  dim(x$table_body[columns])
}
