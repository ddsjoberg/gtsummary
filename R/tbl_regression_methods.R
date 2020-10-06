#' @title Methods for tbl_regression
#'
#' @description Most regression models are handled by [tbl_regression.default],
#' which uses [broom::tidy] to perform initial tidying of results. There are,
#' however, some model types that have modified default printing behavior.
#' Those methods are listed below.
#'
#' @inheritSection tbl_regression Methods
#' @name tbl_regression_methods
#' @rdname tbl_regression_methods
#' @param ... arguments passed to `tbl_regression.default()`
#' @inheritParams tbl_regression
#' @inheritParams tbl_stack
NULL

#' @export
#' @rdname tbl_regression_methods
tbl_regression.lmerMod <- function(
  x, tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"), ...) {
  assert_package("broom.mixed", "tbl_regression.lmerMod")
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.glmerMod <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.survreg <- function(
  x, tidy_fun = function(x, ...) broom::tidy(x, ...) %>% dplyr::filter(.data$term != "Log(scale)"), ...) {
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}
