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
  na_mask <- is.na(x)
  p_fmt <- rep(NA_character_, length(x))
  p_fmt[na_mask] <- na

  # allowing some leeway for numeric storage errors
  out_of_range <- !na_mask & (x > 1 + 1e-15 | x < 0 - 1e-15)

  lt_str <- paste0("<", style_number(
    x = 0.001, digits = 3, big.mark = big.mark,
    decimal.mark = decimal.mark, na = na, ...
  ))

  if (digits == 1) {
    idx <- !na_mask & !out_of_range & x < 0.001
    p_fmt[idx] <- lt_str

    idx <- !na_mask & !out_of_range & x >= 0.001 & !(cards::round5(x, 2) >= 0.1)
    if (any(idx)) {
      p_fmt[idx] <- style_number(x[idx], digits = 3, big.mark = big.mark,
                                 decimal.mark = decimal.mark, na = na, ...)
    }

    idx <- !na_mask & !out_of_range & cards::round5(x, 2) >= 0.1 & !(cards::round5(x, 1) >= 0.2)
    if (any(idx)) {
      p_fmt[idx] <- style_number(x[idx], digits = 2, big.mark = big.mark,
                                 decimal.mark = decimal.mark, na = na, ...)
    }

    idx <- !na_mask & !out_of_range & cards::round5(x, 1) >= 0.2 & x <= 0.9
    if (any(idx)) {
      p_fmt[idx] <- style_number(x[idx], digits = 1, big.mark = big.mark,
                                 decimal.mark = decimal.mark, na = na, ...)
    }

    idx <- !na_mask & !out_of_range & x > 0.9
    p_fmt[idx] <- paste0(">", style_number(
      x = 0.9, digits = 1, big.mark = big.mark,
      decimal.mark = decimal.mark, na = na, ...
    ))
  }
  # rounding large p-values to 2 digits
  else if (digits == 2) {
    idx <- !na_mask & !out_of_range & x < 0.001
    p_fmt[idx] <- lt_str

    idx <- !na_mask & !out_of_range & x >= 0.001 & !(cards::round5(x, 2) >= 0.1)
    if (any(idx)) {
      p_fmt[idx] <- style_number(x[idx], digits = 3, big.mark = big.mark,
                                 decimal.mark = decimal.mark, na = na, ...)
    }

    idx <- !na_mask & !out_of_range & cards::round5(x, 2) >= 0.1 & x <= 0.99
    if (any(idx)) {
      p_fmt[idx] <- style_number(x[idx], digits = 2, big.mark = big.mark,
                                 decimal.mark = decimal.mark, na = na, ...)
    }

    idx <- !na_mask & !out_of_range & x > 0.99
    p_fmt[idx] <- paste0(">", style_number(
      x = 0.99, digits = 2, big.mark = big.mark,
      decimal.mark = decimal.mark, na = na, ...
    ))
  }
  # rounding large pvalues to 3 digit
  else if (digits == 3) {
    idx <- !na_mask & !out_of_range & x < 0.001
    p_fmt[idx] <- lt_str

    idx <- !na_mask & !out_of_range & x >= 0.001 & x <= 0.999
    if (any(idx)) {
      p_fmt[idx] <- style_number(x[idx], digits = 3, big.mark = big.mark,
                                 decimal.mark = decimal.mark, na = na, ...)
    }

    idx <- !na_mask & !out_of_range & x > 0.999
    p_fmt[idx] <- paste0(">", style_number(
      x = 0.999, digits = 3, big.mark = big.mark,
      decimal.mark = decimal.mark, na = na, ...
    ))
  } else {
    cli::cli_abort(
      "The {.arg digits} argument must be one of {.val {1:3}}.",
      call = get_cli_abort_call()
    )
  }

  # prepending a p = in front of value
  if (prepend_p == TRUE) {
    has_ltgt <- !is.na(p_fmt) & grepl(pattern = "<|>", x = p_fmt)
    is_na_val <- is.na(p_fmt) | (!is.na(na) & p_fmt == na)
    idx <- !is_na_val & has_ltgt
    p_fmt[idx] <- paste0("p", p_fmt[idx])
    idx <- !is_na_val & !has_ltgt
    p_fmt[idx] <- paste0("p=", p_fmt[idx])
  }

  attributes(p_fmt) <- attributes(unclass(x))
  return(p_fmt)
}
