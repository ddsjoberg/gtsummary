#' Style percentages
#'
#' @param x numeric vector of percentages
#' @param digits number of digits to round large percentages (i.e. greater than 10%).
#' Smaller percentages are rounded to `digits + 1` places.
#' Default is `0`
#' @param symbol Logical indicator to include percent symbol in output.
#' Default is `FALSE`.
#' @inheritParams style_number
#'
#' @export
#' @return A character vector of styled percentages
#'
#' @author Daniel D. Sjoberg
#' @examples
#' percent_vals <- c(-1, 0, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)
#' style_percent(percent_vals)
#' style_percent(percent_vals, suffix = "%", digits = 1)
style_percent <- function(x,
                          digits = 0,
                          big.mark = ifelse(decimal.mark == ",", " ", ","),
                          decimal.mark = getOption("OutDec"),
                          prefix = "",
                          suffix = "",
                          symbol,
                          ...) {
  set_cli_abort_call()

  # deprecated arguments -------------------------------------------------------
  if (!missing(symbol)) {
    lifecycle::deprecate_soft(
      when = "2.0.3",
      what = "gtsummary::style_percent(symbol)",
      with = I("style_percent(suffix='%')")
    )
    if (isTRUE(symbol)) suffix = "%" # styler: off
  }

  # setting defaults -----------------------------------------------------------
  if (missing(decimal.mark)) {
    decimal.mark <-
      get_theme_element("style_number-arg:decimal.mark", default = decimal.mark)
  }
  if (missing(big.mark)) {
    big.mark <-
      get_theme_element("style_number-arg:big.mark", default = ifelse(decimal.mark == ",", "\U2009", ","))
  }

  y <- dplyr::case_when(
    x * 100 >= 10 ~ style_number(x * 100, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, prefix = prefix, suffix = suffix, ...),
    x * 100 >= 10^(-(digits + 1)) ~ style_number(x * 100, digits = digits + 1, big.mark = big.mark, decimal.mark = decimal.mark, prefix = prefix, suffix = suffix, ...),
    x > 0 ~ paste0("<", style_number(
      x = 10^(-(digits + 1)), digits = digits + 1, big.mark = big.mark,
      decimal.mark = decimal.mark, prefix = prefix, suffix = suffix, ...
    )),
    x == 0 ~ paste0(prefix, "0", suffix)
  )

  attributes(y) <- attributes(unclass(x))
  return(y)
}
