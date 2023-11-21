#' Style p-values
#'
#' @param x Numeric vector of p-values.
#' @param digits Number of digits large p-values are rounded. Must be 1, 2, or 3.
#' Default is 1.
#' @param prepend_p Logical. Should 'p=' be prepended to formatted p-value.
#' Default is `FALSE`
#' @inheritParams style_number
#' @export
#' @return A character vector of styled p-values
#' @family style tools
#' @seealso See tbl_summary \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for examples
#' @author Daniel D. Sjoberg
#' @examples
#' pvals <- c(
#'   1.5, 1, 0.999, 0.5, 0.25, 0.2, 0.197, 0.12, 0.10, 0.0999, 0.06,
#'   0.03, 0.002, 0.001, 0.00099, 0.0002, 0.00002, -1
#' )
#' style_pvalue(pvals)
#' style_pvalue(pvals, digits = 2, prepend_p = TRUE)
style_pvalue <- function(x, digits = 1, prepend_p = FALSE,
                         big.mark = NULL, decimal.mark = NULL, ...) {
  # rounding large p-values to 1 digits
  if (digits == 1) {
    p_fmt <-
      case_when(
        # allowing some leeway for numeric storage errors
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.9 ~ paste0(">", style_number(
          x = 0.9, digits = 1, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        )),
        round2(x, 1) >= 0.2 ~ style_number(x,
          digits = 1, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ),
        round2(x, 2) >= 0.1 ~ style_number(x,
          digits = 2, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ),
        x >= 0.001 ~ style_number(x,
          digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ),
        x < 0.001 ~ paste0("<", style_number(
          x = 0.001, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ))
      )
  }
  # rounding large p-values to 2 digits
  else if (digits == 2) {
    p_fmt <-
      case_when(
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.99 ~ paste0(">", style_number(
          x = 0.99, digits = 2, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        )),
        round2(x, 2) >= 0.1 ~ style_number(x,
          digits = 2, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ),
        x >= 0.001 ~ style_number(x,
          digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ),
        x < 0.001 ~ paste0("<", style_number(
          x = 0.001, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ))
      )
  }

  # rounding large pvalues to 3 digit
  else if (digits == 3) {
    p_fmt <-
      case_when(
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.999 ~ paste0(">", style_number(
          x = 0.999, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        )),
        x >= 0.001 ~ style_number(x,
          digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ),
        x < 0.001 ~ paste0("<", style_number(
          x = 0.001, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, ...
        ))
      )
  } else {
    stop("The `digits=` argument must be 1, 2, or 3.")
  }

  # prepending a p = in front of value
  if (prepend_p == TRUE) {
    p_fmt <- case_when(
      is.na(p_fmt) ~ NA_character_,
      stringr::str_sub(p_fmt, end = 1L) %in% c("<", ">") ~ paste0("p", p_fmt),
      TRUE ~ paste0("p=", p_fmt)
    )
  }

  attributes(p_fmt) <- attributes(unclass(x))
  return(p_fmt)
}
