#' Style numbers
#'
#' @param x Numeric vector
#' @param digits Integer or vector of integers specifying the number of decimals
#' to round `x=`. When vector is passed, each integer is mapped 1:1 to the
#' numeric values in `x`
#' @param big.mark Character used between every 3 digits to separate
#' hundreds/thousands/millions/etc.
#' Default is `","`, except when `decimal.mark = ","` when the default is a space.
#' @param decimal.mark The character to be used to indicate the numeric decimal point.
#' Default is `"."`  or `getOption("OutDec")`
#' @param scale A scaling factor: x will be multiplied by scale before formatting.
#' @param ... Other arguments passed on to `base::format()`
#'
#' @return formatted character vector
#' @export
#' @family style tools
#' @examples
#' c(0.111, 12.3) %>% style_number(digits = 1)
#' c(0.111, 12.3) %>% style_number(digits = c(1, 0))
style_number <- function(x, digits = 0, big.mark = NULL, decimal.mark = NULL,
                         scale = 1, ...) {
  # setting defaults -----------------------------------------------------------
  decimal.mark <-
    decimal.mark %||%
    get_theme_element("style_number-arg:decimal.mark",
      default = getOption("OutDec", default = ".")
    )
  big.mark <-
    big.mark %||%
    get_theme_element("style_number-arg:big.mark",
      # if decimal is a comma, then making big.mark a thin space, otherwise a comma
      default = ifelse(identical(decimal.mark, ","), "\U2009", ",")
    )

  digits <- rep(digits, length.out = length(x))

  ret <-
    map2_chr(
      x, digits,
      function(.x, .y) {
        round2(.x * scale, digits = .y) %>%
          format(
            big.mark = big.mark, decimal.mark = decimal.mark, nsmall = .y,
            scientific = FALSE, trim = TRUE, ...
          )
      }
    )
  ret[is.na(x)] <- NA_character_
  attributes(ret) <- attributes(unclass(x))

  ret
}

# this function assures that 5s are rounded up (and not to even, the default in `round()`)
# code taken from https://github.com/sfirke/janitor/blob/main/R/round_half_up.R
round2 <- function(x, digits = 0) {
  trunc(abs(x) * 10 ^ digits + 0.5 + sqrt(.Machine$double.eps)) / 10 ^ digits * sign(as.numeric(x))
}
