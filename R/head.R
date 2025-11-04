#' Display the first or last rows of a {gtsummary} table
#'
#' @description
#' These functions allow you to view the first (`head()`) or last (`tail()`) `n`
#' rows of a {gtsummary} table.
#'
#' @param x A `gtsummary` object.
#' @param n Number of rows to return. Default is `6L` as default in [utils::head()] and
#'   [utils::tail()].
#' @param ... Additional arguments passed to [utils::head()] or [utils::tail()].
#'
#' @return A `gtsummary` object with only the first or last `n` rows in `table_body`.
#'
#' @examples
#' # head() example
#' trial |>
#'   tbl_summary(by = trt) |>
#'   head(n = 2L)
#'
#' @keywords internal
#' @export
head.gtsummary <- function(x, n = 6L, ...) {
  x$table_body <- head(x$table_body, n = n, ...) # data.frame head
  x
}

#' @examples
#' # tail() example
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tail(n = 2L)
#'
#' @rdname head.gtsummary
#' @keywords internal
#' @export
tail.gtsummary <- function(x, n = 6L, ...) {
  x$table_body <- tail(x$table_body, n = n, ...) # data.frame tail
  x
}
