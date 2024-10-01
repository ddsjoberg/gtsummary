#' Style numbers
#'
#' @param x (`numeric`)\cr
#'   Numeric vector
#' @param digits (non-negative `integer`)\cr
#'   Integer or vector of integers specifying the number of decimals
#'   to round `x`. When vector is passed, each integer is mapped 1:1 to the
#'   numeric values in `x`
#' @param big.mark (`string`)\cr
#'   Character used between every 3 digits to separate
#'   hundreds/thousands/millions/etc.
#'   Default is `","`, except when `decimal.mark = ","` when the default is a space.
#' @param decimal.mark (`string`)\cr
#'   The character to be used to indicate the numeric decimal point.
#'   Default is `"."`  or `getOption("OutDec")`
#' @param scale (scalar `numeric`)\cr
#'   A scaling factor: `x` will be multiplied by scale before formatting.
#' @param prefix (`string`)\cr
#'   Additional text to display before the number.
#' @param suffix (`string`)\cr
#'   Additional text to display after the number.
#' @param ... Arguments passed on to `base::format()`
#'
#' @return formatted character vector
#' @export
#'
#' @examples
#' c(0.111, 12.3) |> style_number(digits = 1)
#' c(0.111, 12.3) |> style_number(digits = c(1, 0))
style_number <- function(x,
                         digits = 0,
                         big.mark = ifelse(decimal.mark == ",", " ", ","),
                         decimal.mark = getOption("OutDec"),
                         scale = 1,
                         prefix = "",
                         suffix = "", ...) {
  set_cli_abort_call()
  if (!is_string(prefix) || !is_string(suffix)) {
    cli::cli_abort(
      "Arguments {.arg prefix} and {.arg suffix} must be strings.",
      call = get_cli_abort_call()
    )
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

  digits <- rep(digits, length.out = length(x))

  ret <- rep(NA_character_, length.out = length(x))

  for (d in unique(digits)) {
    idx <- digits %in% d
    ret[idx] <-
      cards::round5(x[idx] * scale, digits = d) |>
      format(
        big.mark = big.mark, decimal.mark = decimal.mark, nsmall = d,
        scientific = FALSE, trim = TRUE, ...
      )
  }
  ret <- paste0(prefix, ret, suffix)
  ret[is.na(x)] <- NA_character_
  attributes(ret) <- attributes(unclass(x))

  ret
}
