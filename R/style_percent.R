#' Style percentages
#'
#' @param x numeric vector of percentages
#' @param symbol Logical indicator to include percent symbol in output.
#' Default is `FALSE`.
#' @inheritParams style_number
#' @export
#' @return A character vector of styled percentages
#' @family style tools
#' @seealso See Table Gallery \href{http://www.danieldsjoberg.com/gtsummary/articles/gallery.html}{vignette} for example
#' @author Daniel D. Sjoberg
#' @examples
#' percent_vals <- c(-1, 0, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)
#' style_percent(percent_vals)
#' style_percent(percent_vals, symbol = TRUE)
style_percent <- function(x, symbol = FALSE, big.mark = NULL, decimal.mark = NULL, ...) {
  # setting defaults -----------------------------------------------------------
  big.mark <- big.mark %||% get_theme_element("style_number-arg:big.mark", default = ",")
  decimal.mark <- decimal.mark %||% get_theme_element("style_number-arg:decimal.mark", default = ".")

  y <- case_when(
    x * 100 >= 10 ~ style_number(x * 100, digits = 0, big.mark = big.mark, decimal.mark = decimal.mark, ...),
    x * 100 >= 0.1 ~ style_number(x * 100, digits = 1, big.mark = big.mark, decimal.mark = decimal.mark, ...),
    x > 0 ~ paste0("<", style_number(x = 0.1, digits = 1, big.mark = big.mark,
                                     decimal.mark = decimal.mark, ...)),
    x == 0 ~ "0"
  )

  # adding percent symbol if requested
  if (symbol == TRUE) y <- ifelse(!is.na(y), paste0(y, "%"), y)
  return(y)
}
