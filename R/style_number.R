#' Style numbers
#'
#' @param x (`numeric`)\cr
#'   Numeric vector
#' @param digits (non-negative `integer`)\cr
#'   Integer or vector of integers specifying the number of decimals
#'   to round `x`. When vector is passed, each integer is mapped 1:1 to the
#'   numeric values in `x`
#' @param big.mark (`string`)\cr
#'   Character used between every 3 digits to separate
#'   hundreds/thousands/millions/etc.
#'   Default is `","`, except when `decimal.mark = ","` when the default is a space.
#' @param decimal.mark (`string`)\cr
#'   The character to be used to indicate the numeric decimal point.
#'   Default is `"."`  or `getOption("OutDec")`
#' @param scale (scalar `numeric`)\cr
#'   A scaling factor: `x` will be multiplied by scale before formatting.
#' @param prefix (`string`)\cr
#'   Additional text to display before the number.
#' @param suffix (`string`)\cr
#'   Additional text to display after the number.
#' @param na (`NA`/`string`)\cr
#'   Character to replace `NA` values with. Default is `NA_character`
#' @param ... Arguments passed on to `base::format()`
#'
#' @return formatted character vector
#' @export
#'
#' @examples
#' c(0.111, 12.3) |> style_number(digits = 1)
#' c(0.111, 12.3) |> style_number(digits = c(1, 0))
style_number <- function(x,
                         digits = 0,
                         big.mark = ifelse(decimal.mark == ",", " ", ","),
                         decimal.mark = getOption("OutDec"),
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         na = NA_character_,
                         ...) {
  set_cli_abort_call()
  if (!is_string(prefix) || !is_string(suffix)) {
    cli::cli_abort(
      "Arguments {.arg prefix} and {.arg suffix} must be strings.",
      call = get_cli_abort_call()
    )
  }

  # setting defaults -----------------------------------------------------------
  if (missing(decimal.mark)) {
    decimal.mark <-
      get_theme_element("style_number-arg:decimal.mark", default = decimal.mark)
  }
  if (missing(big.mark)) {
    big.mark <-
      get_theme_element("style_number-arg:big.mark", default = ifelse(decimal.mark == ",", "\U2009", ","))
  }

  digits <- rep(digits, length.out = length(x))
  na_mask <- is.na(x)

  unique_digits <- unique(digits)
  if (length(unique_digits) == 1L) {
    d <- unique_digits
    ret <- .fast_format(
      cards::round5(x * scale, digits = d),
      digits = d, big.mark = big.mark, decimal.mark = decimal.mark
    )
  } else {
    ret <- character(length(x))
    for (d in unique_digits) {
      idx <- digits == d
      ret[idx] <- .fast_format(
        cards::round5(x[idx] * scale, digits = d),
        digits = d, big.mark = big.mark, decimal.mark = decimal.mark
      )
    }
  }

  if (nzchar(prefix) || nzchar(suffix)) {
    ret <- paste0(prefix, ret, suffix)
  }
  ret[na_mask] <- na
  attributes(ret) <- attributes(unclass(x))

  ret
}

# Uses sprintf() instead of format() for ~7-10x faster number formatting.
# Handles big.mark insertion via regex only for values >= 1000.
.fast_format <- function(x, digits, big.mark = ",", decimal.mark = ".") {
  x[x == 0] <- 0 # convert -0 to 0

  ret <- sprintf(paste0("%.", digits, "f"), x)

  # insert big.mark for numbers >= 1000
  if (nzchar(big.mark) && any(abs(x) >= 1000, na.rm = TRUE)) {
    needs_mark <- !is.na(x) & is.finite(x) & abs(x) >= 1000
    if (any(needs_mark)) {
      mark_pattern <- paste0("\\1", big.mark)
      if (digits > 0) {
        parts <- strsplit(ret[needs_mark], ".", fixed = TRUE)
        int_parts <- gsub("(\\d)(?=(\\d{3})+(?!\\d))", mark_pattern,
                          vapply(parts, `[`, character(1), 1), perl = TRUE)
        frac_parts <- vapply(parts, `[`, character(1), 2)
        ret[needs_mark] <- paste0(int_parts, decimal.mark, frac_parts)
      } else {
        ret[needs_mark] <- gsub("(\\d)(?=(\\d{3})+(?!\\d))", mark_pattern,
                                ret[needs_mark], perl = TRUE)
      }
    }
  }

  # replace "." with decimal.mark for elements not already handled above
  if (decimal.mark != "." && digits > 0) {
    if (!nzchar(big.mark) || !any(abs(x) >= 1000, na.rm = TRUE)) {
      ret <- sub(".", decimal.mark, ret, fixed = TRUE)
    } else {
      small <- is.na(x) | !is.finite(x) | abs(x) < 1000
      ret[small] <- sub(".", decimal.mark, ret[small], fixed = TRUE)
    }
  }

  ret
}
