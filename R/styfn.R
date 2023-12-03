#' Style Functions
#'
#' Similar to the `style_*()` family of functions, but these functions return
#' a `style_*()` **function** rather than performing the styling.
#'
#' @param digits,big.mark,decimal.mark,scale,prepend_p,symbol,... arguments
#' passed to the `style_*()` functions
#'
#' @return a function
#' @name styfn
#' @family style tools
#'
#' @examples
#' my_style <- styfn_number(digits = 1)
#' my_style(3.14)
NULL

#' @rdname styfn
#' @export
styfn_number <- function(digits = 0, big.mark = NULL, decimal.mark = NULL, scale = 1, ...) {
  function(x) style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, scale = scale, ...)
}

#' @rdname styfn
#' @export
styfn_sigfig <- function(digits = 2, scale = 1, big.mark = NULL, decimal.mark = NULL, ...) {
  function(x) style_sigfig(x, digits = digits, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

#' @rdname styfn
#' @export
styfn_pvalue <- function(digits = 1, prepend_p = FALSE, big.mark = NULL, decimal.mark = NULL, ...) {
  function(x) styfn_pvalue(x, digits = digits, prepend_p = prepend_p, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

#' @rdname styfn
#' @export
styfn_ratio <- function(digits = 2, big.mark = NULL, decimal.mark = NULL, ...) {
  function(x) style_ratio(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

#' @rdname styfn
#' @export
styfn_percent <- function(symbol = FALSE, digits = 0, big.mark = NULL, decimal.mark = NULL, ...) {
  function(x) style_percent(x, symbol = symbol, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

