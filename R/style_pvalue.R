#' Style p-values to be displayed in tables or text
#'
#' @param x Numeric vector of p-values.
#' @param digits Number of digits large p-values are rounded. Must be 1 or 2.
#' Default is 1.
#' @param prepend_p Logical. Should 'p=' be prepended to formatted p-value.
#' Default is `FALSE`
#' @export
#' @return A character vector of styled p-values
#' @family style tools
#' @seealso See tbl_summary \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for examples
#' @author Daniel D. Sjoberg
#' @examples
#' pvals <- c(
#'   1.5, 1, 0.999, 0.5, 0.25, 0.2, 0.197, 0.12, 0.10, 0.0999, 0.06,
#'   0.03, 0.002, 0.001, 0.00099, 0.0002, 0.00002, -1
#' )
#' style_pvalue(pvals)
#' style_pvalue(pvals, digits = 2, prepend_p = TRUE)
style_pvalue <- function(x, digits = 1, prepend_p = FALSE) {

  # rounding large pvalues to 2 digits
  if (digits == 2) {
    p_fmt <-
      case_when(
        x > 1 ~ NA_character_,
        x < 0 ~ NA_character_,
        x > 0.99 ~ ">0.99",
        round(x, 2) >= 0.1 ~ sprintf("%.2f", x),
        x >= 0.001 ~ sprintf("%.3f", x),
        x < 0.001 ~ "<0.001"
      )
  }

  # rounding large pvalues to 1 digit
  else if (digits == 1) {
    p_fmt <-
      case_when(
        x > 1 ~ NA_character_,
        x < 0 ~ NA_character_,
        x > 0.9 ~ ">0.9",
        round(x, 1) >= 0.2 ~ sprintf("%.1f", x),
        round(x, 2) >= 0.1 ~ sprintf("%.2f", x),
        x >= 0.001 ~ sprintf("%.3f", x),
        x < 0.001 ~ "<0.001"
      )
  }
  else {
    stop("'digits' argument must be 1 or 2.")
  }

  # prepending a p = in front of value
  if (prepend_p == TRUE) {
    p_fmt <- case_when(
      is.na(p_fmt) ~ NA_character_,
      stringr::str_sub(p_fmt, end = 1L) %in% c("<", ">") ~ paste0("p", p_fmt),
      TRUE ~ paste0("p=", p_fmt)
    )
  }

  return(p_fmt)
}
