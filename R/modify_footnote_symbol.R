#' Modify Footnote Symbols
#'
#' Customize the symbols used to reference footnotes in a gtsummary table. By
#' default, footnotes are referenced with sequential numbers (`1, 2, 3, ...`).
#' This function replaces those numbers with a user-specified set of symbols,
#' e.g. `c("*", "\u2020", "\u2021")`.
#'
#' The symbol sequence may also be set for all tables via the
#' `"pkgwide-chr:footnote_symbol"` theme element; a value set with
#' `modify_footnote_symbol()` takes precedence over the theme element.
#'
#' @inheritParams modify
#' @param symbol (`character`)\cr
#'   a character vector of length 2 or greater giving the ordered symbols used
#'   to reference footnotes, e.g. `c("*", "\u2020", "\u2021")`. Symbols are
#'   assigned to footnotes in the order the footnotes appear in the table. When
#'   the number of footnotes exceeds the number of symbols supplied, the symbols
#'   are recycled. A length of at least 2 is required because `as_gt()` passes
#'   these to `gt::opt_footnote_marks()`, which interprets a single string as
#'   the name of a built-in mark scheme. Currently only utilized by `as_gt()`
#'   and `as_flex_table()`.
#'
#' @return Updated gtsummary object
#' @name modify_footnote_symbol
#' @seealso
#'   [`modify_footnote_header()`], [`modify_footnote_body()`], [`modify_footnote_spanning_header()`]
#'
#'   [Footnotes vs Source Notes vs Abbreviations](https://www.danieldsjoberg.com/gtsummary/articles/modify-functions.html)
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # use symbols (instead of numbers) to reference footnotes
#' trial |>
#'   tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
#'   add_p() |>
#'   modify_footnote_symbol(symbol = c("*", "\u2020", "\u2021"))
NULL

#' @export
#' @rdname modify_footnote_symbol
modify_footnote_symbol <- function(x, symbol) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_not_missing(symbol)
  check_class(symbol, "character")
  if (length(symbol) < 2L) {
    cli::cli_abort(
      "The {.arg symbol} argument must be a character vector of length 2 or greater.",
      call = get_cli_abort_call()
    )
  }
  updated_call_list <- c(x$call_list, list(modify_footnote_symbol = match.call()))

  # store the ordered symbol sequence ------------------------------------------
  x$table_styling$footnote_symbol <- symbol

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}
