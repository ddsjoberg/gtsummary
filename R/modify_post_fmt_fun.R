#' Modify post formatting
#'
#' `r lifecycle::badge('experimental')`\cr
#' Apply a formatting function after the primary formatting functions have been applied.
#' The function is similar to `gt::text_transform()`.
#'
#' @inheritParams modify_footnote_body
#' @param fmt_fun (`function`)\cr
#'   a function that will be applied to the specified columns and rows.
#'
#' @return Updated gtsummary object
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' data.frame(x = FALSE) |>
#'   tbl_summary(type = x ~ "categorical") |>
#'   modify_post_fmt_fun(
#'     fmt_fun = ~ifelse(. == "0 (0%)", "0", .),
#'     columns = all_stat_cols()
#'   )
modify_post_fmt_fun <- function(x, fmt_fun, columns, rows = TRUE) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(fmt_fun)
  check_not_missing(columns)
  check_class(x, "gtsummary")
  fmt_fun <- as_function(fmt_fun, call = get_cli_abort_call())
  cards::process_selectors(scope_header(x$table_body), columns =  {{ columns }})
  .check_rows_input(x, rows = {{ rows }})
  rows <- enquo(rows)

  # return table if no columns selected ----------------------------------------
  if (is_empty(columns)) {
    return(x)
  }

  # save the information in a table to be executed after fmt_funs have been applied
  x$table_styling$post_fmt_fun <-
    x$table_styling$post_fmt_fun |>
    dplyr::bind_rows(
      dplyr::tibble(
        column = columns,
        rows = list(rows),
        fmt_fun = list(fmt_fun)
      )
    )

  # return table ---------------------------------------------------------------
  x
}
