#' Implement significant figure-like rounding
#'
#' Converts a numeric argument into a string that has been rounded to a
#' significant figure-like number. Scientific notation output
#' is avoided, however, and additional significant figures may be displayed for
#' large numbers.  For example, if the number of significant digits
#' requested is 2, 123 will be displayed (rather than 120 or 1.2x10^2).
#'
#' If 2 sig figs are input, the number is rounded to 2 decimal places
#' when `abs(x) < 1`, 1 decimal place when `abs(x) >= 1 & abs(x) < 10`,
#' and to the nearest integer when `abs(x) >= 10`.
#'
#' @param x Numeric vector
#' @param digits Integer specifying the minimum number of significant
#' digits to display
#' @export
#' @return A character vector of styled numbers
#' @family style tools
#' @author Daniel D. Sjoberg
#' @examples
#' c(0.123, 0.9, 1.1234, 12.345, -0.123, -0.9, -1.1234, -12.345, NA, -0.001) %>%
#'   style_sigfig()
style_sigfig <- function(x, digits = 2) {

  # the purrr portion creates a list of length {digits} with each
  # condition for rounding.  The are in the order they are run.
  # They are in the format of case_when
  map(
    digits:1,
    ~ glue("abs(x) < 10^{digits - .x} ~ sprintf('%.{.x}f', x)")
  ) %>%
    # pasting together all conditions
    paste(collapse = ", ") %>%
    # adding the case_when function, as well as a final
    # condition to round to nearest integer
    {
      glue("case_when({.}, TRUE ~ ifelse(is.na(x), NA_character_, sprintf('%.0f', x)))")
    } %>%
    # converting strings into expressions to run
    parse(text = .) %>%
    eval() %>%
    # converting "-0.000" type values to "0.000"
    {
      ifelse(
        as.numeric(.) == 0 & str_starts(., pattern = "-"),
        str_remove(., pattern = "-"),
        .
      )
    }
}
