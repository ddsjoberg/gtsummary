#' Tidies regression object based on class
#'
#' The `tidy_wrap()` function has two primary functions.  First, using either
#' `broom::tidy` or `broom.mixed::tidy` (depending on model input class)
#' the regression model object is converted into a data frame. It then adjusts the
#' output for use in the rest of \code{\link{fmt_regression}}.
#'
#' The output of `broom::tidy` or `broom.mixed::tidy` will often include additional information
#' that will not be included in a printed table from `fmt_regression()`
#' (e.g. scale parameters, random effects, etc.).  This
#' simple helper function deletes extraneous rows from the output.
#' It also adds a column of NAs if the model does not calculate p.values, since
#' the rest of `fmt_regression()` depends on having a column called p.value.
#'
#' @param x regression model object
#' @param exponentiate logical argument passed directly to `broom::tidy`
#' or `broom.mixed::tidy`.
#' @param conf.level confidence level passed directly to `broom::tidy`
#' or `broom.mixed::tidy`.
#' @keywords internal

# Points function to use mixed vs non-mixed version of broom
tidy_wrap <- function(x, exponentiate, conf.level) {
  mixed_classes <- c("lmerMod", "glmerMod", "nlme")
  if (class(x)[1] %in% mixed_classes) { # can add other classes later. Need exact subclass.
    tidy_bit <- broom.mixed::tidy(
      x,
      exponentiate = exponentiate,
      conf.level = conf.level, conf.int = T, effects = "fixed"
    )
  }

  if (!(class(x)[1] %in% mixed_classes)) {
    tidy_bit <- broom::tidy(
      x,
      exponentiate = exponentiate,
      conf.level = conf.level, conf.int = T
    )
  }

  # deleting scale parameters from survreg objects
  if (class(x)[1] == "survreg") {
    return(
      tidy_bit %>%
        dplyr::filter_(~ term != "Log(scale)")
    )
  }

  # looks for if p.value column is missing and adds NAs if so
  missed <- base::setdiff("p.value", names(tidy_bit))
  tidy_bit[missed] <- NA

  # otherwise returning original output
  return(tidy_bit)
}
