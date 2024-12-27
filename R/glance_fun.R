#' Default glance function
#'
#' This is an S3 generic used as the default function in `add_glance*(glance_fun)`.
#' It's provided so various regression model classes can have their own default
#' functions for returning statistics.
#'
#' @param x (regression model)\cr
#'   a regression model object
#' @inheritParams rlang::args_dots_empty
#'
#' @return a function
#' @name glance_fun_s3
#' @keywords internal
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("cardx")
#' mod <- lm(age ~ trt, trial)
#'
#' glance_fun_s3(mod)
NULL

#' @name glance_fun_s3
#' @export
glance_fun_s3 <- function(x, ...) {
  UseMethod("glance_fun_s3")
}

#' @name glance_fun_s3
#' @export
glance_fun_s3.default <- function(x, ...) {
  check_pkg_installed("broom")
  check_dots_empty()
  broom::glance
}

#' @name glance_fun_s3
#' @export
glance_fun_s3.mira <- function(x, ...) {
  check_pkg_installed(c("broom",  "mice"))
  check_dots_empty()
  \(x) suppressWarnings(broom::glance(mice::pool(x)))
}
