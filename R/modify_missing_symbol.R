#' Modify Missing Substitution
#'
#' Specify how missing values will be represented in the printed table.
#' By default, a blank space is printed for all `NA` values.
#'
#' @inheritParams modify_footnote2
#' @param symbol (`string`)\cr
#'   string indicating how missing values are formatted.
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to add missing symbol.
#'
#' @return Updated gtsummary object
#' @export
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Use the abbreivation "Ref." for reference rows instead of the em-dash
#' lm(marker ~ trt, data = trial) |>
#'   tbl_regression() |>
#'   modify_missing_symbol(
#'     symbol = "Ref.",
#'     columns = c(estimate, conf.low, conf.high),
#'     rows = reference_row == TRUE
#'   )
modify_missing_symbol <- function(x, symbol, columns, rows) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(columns)
  check_not_missing(rows)
  check_not_missing(symbol)
  check_class(x, "gtsummary")
  check_string(symbol)
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  x$call_list <- c(x$call_list, list(modify_missing_symbol = match.call()))
  .modify_missing_symbol(x = x, symbol = symbol, columns = columns, rows = {{ rows }})
}

.modify_missing_symbol <- function(x, symbol, columns, rows) {
  # add updates to `x$table_styling$fmt_missing` -------------------------------
  x$table_styling$fmt_missing <- x$table_styling$fmt_missing |>
    dplyr::bind_rows(
      tidyr::expand_grid(
        column = columns,
        rows = list(enquo(rows)),
        symbol = symbol
      )
    )

  # return table ---------------------------------------------------------------
  x
}
