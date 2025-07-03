#' Deprecated functions
#'
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# "soft" deprecation for 6 months: (Sys.Date() - lubridate::dmonths(6)) |> as.Date()
#   v2.2.0 2025-04-11
#   v2.1.0 2025-02-19
#   v2.0.2 2024-09-05
#   v2.0.1 2024-08-01

# "warn" deprecation for 18 months: (Sys.Date() - lubridate::dmonths(24)) |> as.Date()
#   v2.0.0 2024-07-23
#   v1.7.2 2023-07-13

# "stop" deprecation for 12 months: (Sys.Date() - lubridate::dmonths(36)) |> as.Date()
#   v1.7.1 2023-04-27
#   v1.7.0 2023-01-13
#   v1.6.3 2022-12-06
#   v1.6.2 2022-09-30


# v2.3.0 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
modify_column_indent <- function(...) {
  lifecycle::deprecate_soft("2.3.0", "gtsummary::modify_column_indent()", "modify_indent()")
  modify_indent(...)
}

# v2.3.0 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
tbl_split <- function(x, ...) {
  UseMethod("tbl_split")
}

# v2.3.0 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
tbl_split.gtsummary <- function(...) {
  lifecycle::deprecate_soft("2.3.0", "gtsummary::tbl_split.gtsummary()", "tbl_split_by_rows()")
  tbl_split_by_rows(...)
}
