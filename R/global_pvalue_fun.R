#' Global p-value generic
#'
#' @description
#' An S3 generic that serves as the default for `add_global_p(anova_fun)`.
#'
#' The default function uses `car::Anova()` (via [`cardx::ard_car_anova()`]) to
#' calculate the p-values.
#'
#' The method for GEE models (created from `geepack::geeglm()`) returns Wald tests calculated
#' using `aod::wald.test()` (via [`cardx::ard_aod_wald_test()`]). For this method,
#' the `type` argument is not used.
#'
#' @name global_pvalue_fun
#' @return data frame
#' @keywords internal
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("cardx", "broom", "car", "parameters"))
#' lm(age ~ stage + grade, trial) |>
#'   global_pvalue_fun(type = "III")
NULL

#' @rdname global_pvalue_fun
#' @export
global_pvalue_fun <- function(x, type, ...) {
  UseMethod("global_pvalue_fun")
}

#' @rdname global_pvalue_fun
#' @export
global_pvalue_fun.default <- function(x, type, ...) {
  check_pkg_installed(c("cardx"))

  cardx::ard_car_anova(x, type = type, ...)
}

#' @rdname global_pvalue_fun
#' @export
global_pvalue_fun.geeglm <- function(x, type, ...) {
  check_pkg_installed(c("cardx"))

  cardx::ard_aod_wald_test(x, ...)
}
