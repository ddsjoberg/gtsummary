#' Creates table of univariate summary statistics for time-to-event endpoints
#'
#' \lifecycle{deprecated}
#' Please use [tbl_survfit].
#' @param ... Not used
#'
#' @keywords internal
#' @name tbl_survival

NULL

#' @rdname tbl_survival
#' @export
tbl_survival <- function(...) {
  lifecycle::deprecate_stop(
    when = "1.4.0",
    what = "gtsummary::tbl_survival()",
    with = "tbl_survfit()"
  )
}
