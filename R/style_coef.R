#' Implement significant figure like formatting for regression coefficients
#'
#' Converts a numeric argument into a string that has been rounded to a
#' significant figure-like number suitable for regresion model coefficients.
#' Values with magnitude less than 1, are rounded to two significant figures, and magnitudes
#' larger than 1 are rounded to three significant figures
#'
#' @param x numeric vector
#' @export
#' @seealso style_sigfig
#' @author Daniel Sjoberg
#' @examples
#' c(0.123, 0.9, 1.1234, 12.345, 101.234, -0.123, -0.9, -1.1234, -12.345, -101.234) %>%
#'   style_coef()

style_coef <- function(x) {
  ifelse(
    abs(x) < 1,
    style_sigfig(x, digits = 2),
    style_sigfig(x, digits = 3)
  )
}

