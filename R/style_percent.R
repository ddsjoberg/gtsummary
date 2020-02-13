#' Style percentages to be displayed in tables or text
#'
#' @param x numeric vector of percentages
#' @param symbol Logical indicator to include percent symbol in output.
#' Default is `FALSE`.
#' @export
#' @return A character vector of styled percentages
#' @family style tools
#' @seealso See Table Gallery \href{http://www.danieldsjoberg.com/gtsummary/articles/gallery.html}{vignette} for example
#' @author Daniel D. Sjoberg
#' @examples
#' percent_vals <- c(-1, 0, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)
#' style_percent(percent_vals)
#' style_percent(percent_vals, symbol = TRUE)
style_percent <- function(x, symbol = FALSE) {
  y <- case_when(
    x >= 0.10 ~ sprintf("%.0f", x * 100),
    x >= 0.001 ~ sprintf("%.1f", x * 100),
    x > 0 ~ "<0.1",
    x == 0 ~ "0"
  )

  # adding percent symbol if requested
  if (symbol == TRUE) y <- ifelse(!is.na(y), paste0(y, "%"), y)
  return(y)
}
