#' @title `rows` argument
#' @keywords internal
#' @name rows_argument
#'
#' @description
#' The rows argument accepts a predicate expression that is used to specify
#' rows to apply formatting. The expression must evaluate to a logical when
#' evaluated in `x$table_body`. For example, to apply formatting to the age rows
#' pass `rows = variable == "age"`. A vector of row numbers is NOT acceptable.
#'
#' A couple of things to note when using the `rows` argument.
#' 1. You can use saved objects to create the predicate argument, e.g.
#'   `rows = variable == letters[1]`.
#' 2. The saved object cannot share a name with a column in `x$table_body`.
#'   The reason for this is that in `tbl_merge()` the columns are renamed,
#'   and the renaming process cannot disambiguate the `variable` column from
#'   an external object named `variable` in the following expression
#'   `rows = .data$variable = .env$variable`.
NULL
