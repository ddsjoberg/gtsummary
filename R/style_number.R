#' Style numbers
#'
#' @param x Numeric vector
#' @param digits Integer or vector of integers specifying the number of digits
#' to round `x=`. When vector is passed, each integer is mapped 1:1 to the
#' numeric values in `x`
#' @param big.mark Character used between every 3 digits to separate thousands.
#' Default is `","`
#' @param decimal.mark The character to be used to indicate the numeric decimal point.
#' Default is `"."`
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
  big.mark <- big.mark %||% get_theme_element("style_number-arg:big.mark", default = ",")
  decimal.mark <- decimal.mark %||% get_theme_element("style_number-arg:decimal.mark", default = ".")

  digits <- rep(digits, length.out = length(x))

  ret <-
    map2_chr(
      x, digits,
      function(.x, .y) {
        round(.x * scale, digits = .y) %>%
        format(big.mark = big.mark, decimal.mark = decimal.mark, nsmall = .y,
               scientific = FALSE, trim = TRUE, ...)
      }
    )
  ret[is.na(x)] <- NA_character_
  names(ret) <- names(x)

  ret
}
