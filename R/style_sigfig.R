#' Style significant figure-like rounding
#'
#' Converts a numeric argument into a string that has been rounded to a
#' significant figure-like number. Scientific notation output
#' is avoided, however, and additional significant figures may be displayed for
#' large numbers.  For example, if the number of significant digits
#' requested is 2, 123 will be displayed (rather than 120 or 1.2x10^2).
#'
#' @section Details:
#'
#' - Scientific notation output is avoided.
#'
#' - If 2 significant figures are requested, the number is rounded to no more than 2 decimal places.
#'   For example, a number will be rounded to 2 decimals places when `abs(x) < 1`,
#'   1 decimal place when `abs(x) >= 1 & abs(x) < 10`,
#'   and to the nearest integer when `abs(x) >= 10`.
#'
#' - Additional significant figures
#'   may be displayed for large numbers. For example, if the number of
#'   significant digits requested is 2,
#'   123 will be displayed (rather than 120 or 1.2x10^2).
#'
#' @param x Numeric vector
#' @param digits Integer specifying the minimum number of significant
#' digits to display
#' @inheritParams style_number
#' @export
#' @return A character vector of styled numbers
#' @family style tools
#' @author Daniel D. Sjoberg
#' @examples
#' c(0.123, 0.9, 1.1234, 12.345, -0.123, -0.9, -1.1234, -132.345, NA, -0.001) %>%
#'   style_sigfig()
style_sigfig <- function(x,
                         digits = 2,
                         scale = 1,
                         big.mark = ifelse(decimal.mark == ",", " ", ","),
                         decimal.mark = getOption("OutDec"),
                         prefix = "",
                         suffix = "",
                         na = NA_character_,
                         ...) {
  set_cli_abort_call()

  # setting defaults -----------------------------------------------------------
  if (missing(decimal.mark)) {
    decimal.mark <-
      get_theme_element("style_number-arg:decimal.mark", default = decimal.mark)
  }
  if (missing(big.mark)) {
    big.mark <-
      get_theme_element("style_number-arg:big.mark", default = ifelse(decimal.mark == ",", "\U2009", ","))
  }

  # calculating the number of digits to round number
  # process from least-specific (fewest digits) to most-specific so later
  # assignments overwrite earlier ones, matching the original case_when order
  d <- rep(0L, length(x))
  digit_seq <- digits:1  # e.g. c(2, 1) for digits=2
  for (i in rev(seq_along(digit_seq))) {
    dig <- digit_seq[i]
    threshold <- 10^(i - 1) - 0.5 * 10^(-dig)
    mask <- cards::round5(abs(x * scale), digits = dig + 1L) < threshold
    mask[is.na(mask)] <- FALSE
    d[mask] <- dig
  }

  # formatting number
  style_number(x, digits = d, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, prefix = prefix, suffix = suffix, na = na, ...)
}
