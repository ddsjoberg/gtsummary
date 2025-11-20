#' Display the first or last rows
#'
#' @description
#' These functions allow you to view the first (`head()`) or last (`tail()`) `n`
#' rows of a gtsummary table.
#'
#' @param x A `gtsummary` object.
#' @param n Number of rows to return. Default is `6L` as default in [utils::head()] and
#'   [utils::tail()].
#' @param ... Additional arguments passed to [utils::head()], [utils::tail()], or `[`.
#'
#' @return A `gtsummary` object with only the first or last `n` rows in `table_body`.
#' @keywords internal
#' @name head
#'
#' @examples
#' tbl <- tbl_summary(trial, by = trt, include = grade)
#'
#' # Example 1 head() ---------------------------
#' head(tbl, n = 2L)
#'
#' # Example 2 tail() ---------------------------
#' tail(tbl, n = 2L)
#'
#' # Example 3 `[`() ---------------------------
#' tbl[1:2,]
NULL

#' @rdname head
#' @export
head.gtsummary <- function(x, n = 6L, ...) {
  x$table_body <- head(x$table_body, n = n, ...) # data.frame head
  x
}

#' @rdname head
#' @export
tail.gtsummary <- function(x, n = 6L, ...) {
  x$table_body <- tail(x$table_body, n = n, ...) # data.frame tail
  x
}

#' @rdname head
#' @export
`[.gtsummary` <- function(x, ...) {
  x$table_body <- x$table_body[...]
  x
}
