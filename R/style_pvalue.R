#' Style p-values
#'
#' @param x (`numeric`)\cr
#'   Numeric vector of p-values.
#' @param digits (`integer`)\cr
#'   Number of digits large p-values are rounded. Must be 1, 2, or 3.
#'   Default is 1.
#' @param prepend_p (scalar `logical`)\cr
#'   Logical. Should 'p=' be prepended to formatted p-value.
#'   Default is `FALSE`
#' @inheritParams style_number
#'
#' @export
#' @return A character vector of styled p-values
#'
#' @author Daniel D. Sjoberg
#' @examples
#' pvals <- c(
#'   1.5, 1, 0.999, 0.5, 0.25, 0.2, 0.197, 0.12, 0.10, 0.0999, 0.06,
#'   0.03, 0.002, 0.001, 0.00099, 0.0002, 0.00002, -1
#' )
#' style_pvalue(pvals)
#' style_pvalue(pvals, digits = 2, prepend_p = TRUE)
style_pvalue <- function(x,
                         digits = 1,
                         prepend_p = FALSE,
                         big.mark = ifelse(decimal.mark == ",", " ", ","),
                         decimal.mark = getOption("OutDec"),
                         na = NA_character_,
                         ...) {
  set_cli_abort_call()

  # setting defaults -----------------------------------------------------------
  if (missing(decimal.mark)) {
    decimal.mark <-
      get_theme_element("style_number-arg:decimal.mark", default = decimal.mark)
  }
  if (missing(big.mark)) {
    big.mark <-
      get_theme_element("style_number-arg:big.mark", default = ifelse(decimal.mark == ",", "\U2009", ","))
  }

  # rounding large p-values to 1 digits
  if (digits == 1) {
    p_fmt <-
      dplyr::case_when(
        # allowing some leeway for numeric storage errors
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.9 ~ paste0(">", style_number(
          x = 0.9, digits = 1, big.mark = big.mark,
          decimal.mark = decimal.mark, na = na, ...
        )),
        cards::round5(x, 1) >= 0.2 ~ style_number(x,
                                                  digits = 1, big.mark = big.mark,
                                                  decimal.mark = decimal.mark, na = na, ...
        ),
        cards::round5(x, 2) >= 0.1 ~ style_number(x,
                                                  digits = 2, big.mark = big.mark,
                                                  decimal.mark = decimal.mark, na = na, ...
        ),
        x >= 0.001 ~ style_number(x,
                                  digits = 3, big.mark = big.mark,
                                  decimal.mark = decimal.mark, na = na, ...
        ),
        x < 0.001 ~ paste0("<", style_number(
          x = 0.001, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, na = na, ...
        )),
        is.na(x) ~ na
      )
  }
  # rounding large p-values to 2 digits
  else if (digits == 2) {
    p_fmt <-
      dplyr::case_when(
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.99 ~ paste0(">", style_number(
          x = 0.99, digits = 2, big.mark = big.mark,
          decimal.mark = decimal.mark, na = na, ...
        )),
        cards::round5(x, 2) >= 0.1 ~ style_number(x,
                                                  digits = 2, big.mark = big.mark,
                                                  decimal.mark = decimal.mark, na = na, ...
        ),
        x >= 0.001 ~ style_number(x,
                                  digits = 3, big.mark = big.mark,
                                  decimal.mark = decimal.mark, na = na, ...
        ),
        x < 0.001 ~ paste0("<", style_number(
          x = 0.001, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, na = na, ...
        )),
        is.na(x) ~ na
      )
  }

  # rounding large pvalues to 3 digit
  else if (digits == 3) {
    p_fmt <-
      dplyr::case_when(
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.999 ~ paste0(">", style_number(
          x = 0.999, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, na = na, ...
        )),
        x >= 0.001 ~ style_number(x,
                                  digits = 3, big.mark = big.mark,
                                  decimal.mark = decimal.mark, na = na, ...
        ),
        x < 0.001 ~ paste0("<", style_number(
          x = 0.001, digits = 3, big.mark = big.mark,
          decimal.mark = decimal.mark, na = na, ...
        )),
        is.na(x) ~ na
      )
  } else {
    cli::cli_abort(
      "The {.arg digits} argument must be one of {.val {1:3}}.",
      call = get_cli_abort_call()
    )
  }

  # prepending a p = in front of value
  if (prepend_p == TRUE) {
    p_fmt <- dplyr::case_when(
      is.na(p_fmt) ~ NA_character_,
      (is.na(na) & is.na(p_fmt)) | p_fmt == na ~ na,
      grepl(pattern = "<|>", x = p_fmt) ~ paste0("p", p_fmt),
      TRUE ~ paste0("p=", p_fmt)
    )
  }

  attributes(p_fmt) <- attributes(unclass(x))
  return(p_fmt)
}
