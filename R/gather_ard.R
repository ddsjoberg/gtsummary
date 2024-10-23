#' Extract ARDs
#'
#' Extract the ARDs from a gtsummary table. If needed, results may be combined
#' with `cards::bind_ard()`.
#'
#' @param x (`gtsummary`)\cr a gtsummary table.
#'
#' @return list
#' @export
#'
#' @examplesIf gtsummary:::is_pkg_installed('cardx')
#' tbl_summary(trial, by = trt, include = age) |>
#'   add_overall() |>
#'   add_p() |>
#'   gather_ard()
#'
#' glm(response ~ trt, data = trial, family = binomial()) |>
#'   tbl_regression() |>
#'   gather_ard()
gather_ard <- function(x) {
  check_not_missing(x)
  check_class(x, "gtsummary")

  # cycle through the underlying tbls, and get ARD from each
  if (inherits(x, c("tbl_stack", "tbl_merge", "tbl_uvregression"))) {
    return(map(x[["tbls"]], gather_ard))
  }

  # the ARD for regression models is an additional calculation, so we don't do it by default
  if (inherits(x, "tbl_regression")) {
    check_pkg_installed("cardx")
    return(cardx::ard_regression(x$inputs$x))
  }

  # grab ARD from standard place
  if (!is_empty(x[["cards"]])) return(x[["cards"]]) # styler: off

  # if no ARD found, print note
  cli::cli_inform("No ARD found for {.cls {class(x)}} table.")
  invisible()
}
