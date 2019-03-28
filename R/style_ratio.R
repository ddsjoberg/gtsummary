#' Implement significant figure-like formatting for ratios
#'
#' When reporting ratios, such as relative risk, or an odds ratio, we'll often
#' want the rounding to be similar on each side of the number 1.  For example,
#' if we report an odds ratio of 0.95 with a confidence interval of 0.70 to 1.24,
#' we would want to round to two decimal places for all values. In other words,
#' 2 significant figures for numbers less than 1 and 3 significant figures 1 and
#' larger. `style_ratio()` performs significant figure-like formatting in this manner.
#'
#' @param x numeric vector
#' @param digits integer specifying the number of significant
#' digits to display for numbers below 1. Numbers larger than 1 will be be `digits + 1`.
#' Default is `digits = 2`.
#' @export
#' @seealso \code{\link{style_sigfig}}
#' @author Daniel Sjoberg
#' @examples
#' c(0.123, 0.9, 1.1234, 12.345, 101.234, -0.123,
#'   -0.9, -1.1234, -12.345, -101.234) %>%
#'   style_ratio()

style_ratio <- function(x, digits = 2) {
  ifelse(
    abs(x) < 1,
    style_sigfig(x, digits = digits),
    style_sigfig(x, digits = digits +  1)
  )
}

