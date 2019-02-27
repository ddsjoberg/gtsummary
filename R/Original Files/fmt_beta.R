#' Round and format regression model coefficients
#'
#' Takes in a vector of regression model coefficients, and returns a
#' rounded and formatted coefficient vector.  This is the default
#' rounding for models formatted with `biostatR::fmt_regression()`.  Coefficients
#' larger than 100 are rounded to the nearest integer, between  10 and 100 to one
#' decimal place, and all others to 2 decimal places.  These thresholds were
#' selected with odds ratios and hazard ratios in mind.
#'
#' @param x numeric vector
#' @export
#' @examples
#' or <- c(100.2342, 11.234, 5.32423, 1.23423469, 0.234)
#' fmt_beta(or)
fmt_beta <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 100 ~ sprintf("%.0f", x),
    abs(x) >= 10 ~ sprintf("%.1f", x),
    TRUE ~ sprintf("%.2f", x)
  )
}
