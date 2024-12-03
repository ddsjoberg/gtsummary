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
#  v2.0.2 2024-09-05
#  v2.0.1 2024-08-01
#  v2.0.0 2024-07-23

# "warn" deprecation for 18 months: (Sys.Date() - lubridate::dmonths(24)) |> as.Date()
#   v1.7.2 2023-07-13
#   v1.7.1 2023-04-27

# "stop" deprecation for 12 months: (Sys.Date() - lubridate::dmonths(36)) |> as.Date()
#   v1.7.0 2023-01-13
#   v1.6.3 2022-12-06
#   v1.6.2 2022-09-30
#   v1.6.1 2022-06-22
#   v1.6.0 2022-04-25
#   v1.5.2 2022-01-29
#   v1.5.1 2022-01-20
#   v1.5.0 2021-10-16


# v1.6.1 ----------------------------------------------------------
#' @rdname deprecated
#' @export
modify_cols_merge <- function(...) {
  lifecycle::deprecate_stop("1.6.1", "gtsummary::modify_cols_merge()", "modify_column_merge()")
}
