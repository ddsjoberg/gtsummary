#' Display the first rows of a gtsummary table
#'
#' @param x A `gtsummary` object.
#' @param n Number of rows to return. Default is `6L` as default in `head()`.
#' @param ... Additional arguments passed to `head()`.
#'
#' @return A `gtsummary` object with only the first `n` rows in `table_body`.
#'
#' @examples
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tail(n = 2L)
#'
#' @export
head.gtsummary <- function(x, n = 6L, ...) {
  set_cli_abort_call()
  if (!inherits(x, "gtsummary")) {
    cli::cli_abort("{.arg x} must be a gtsummary object.", call = get_cli_abort_call())
  }
  x$table_body <- head(x$table_body, n = n, ...) # data.frame head
  x
}

#' Display the last rows of a gtsummary table
#'
#' @param x A `gtsummary` object.
#' @param n Number of rows to return. Default is `6L` as default in `tail()`.
#' @param ... Additional arguments passed to `tail()`.
#'
#' @return A `gtsummary` object with only the last `n` rows in `table_body`.
#'
#' @examples
#' trial |>
#'   tbl_summary(by = trt) |>
#'   tail(n = 2L)
#'
#'   @export
tail.gtsummary <- function(x, n = 6L, ...) {
  set_cli_abort_call()
  if (!inherits(x, "gtsummary")) {
    cli::cli_abort("{.arg x} must be a gtsummary object.", call = get_cli_abort_call())
  }
  x$table_body <- tail(x$table_body, n = n, ...) # data.frame tail
  x
}
