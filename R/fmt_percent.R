#' Formats percentages to be displayed in tables or text of report.
#'
#' @param x numeric vector of percentages
#' @param symbol logical indicator to include percent symbol in output.  Default is FALSE.
#' @return Formatted percentages
#' @export
#' @examples
#' percent_vals <- c(-1, 0, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)
#' fmt_percent(percent_vals)
#' fmt_percent(percent_vals, symbol = TRUE)
fmt_percent <- function(x, symbol = FALSE) {
  y <- dplyr::case_when(
    x >= 0.10 ~ sprintf("%.0f", x * 100),
    x >= 0.001 ~ sprintf("%.1f", x * 100),
    x > 0 ~ "<0.1",
    x == 0 ~ "0"
  )

  # adding percent symbol if requested
  if (symbol == TRUE) y <- ifelse(!is.na(y), paste0(y, "%"), y)
  return(y)
}
