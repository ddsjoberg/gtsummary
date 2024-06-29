#' Deprecated functions
#'
#' \lifecycle{deprecated}
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# tentative deprecation schedule, `Sys.Date() - months(18)`
# "warn" for 18 months
#   2.0.0 TARGET 2024-07-01 TODO: Update this
#   1.7.2 2023-07-13
#   1.7.1 2023-04-27
#   1.7.0 2023-01-13


# "stop" for 18 months, then delete from pkg, `Sys.Date() - months(36)`
#   1.6.3 2022-12-06
#   1.6.2 2022-09-30
#   1.6.1 2022-06-22
#   1.6.0 2022-04-25
#   1.5.2 2022-01-29
#   1.5.1 2022-01-20
#   1.5.0 2021-10-16
#   1.4.2 2021-07-13


# TODO: Delete these version and dates
#   1.4.1 2021-05-19
#   1.4.0 2021-04-13
#   1.3.7 2021-02-26
#   1.3.6 2021-01-08
#   1.3.5 2020-09-29
#   1.3.4 2020-08-27
#   1.3.3 2020-08-11

# v1.6.1 ----------------------------------------------------------
#' @rdname deprecated
#' @export
modify_cols_merge <- function(...) {
  lifecycle::deprecate_stop("1.6.1", "gtsummary::modify_cols_merge()", "modify_column_merge()")
}
