#' Modify Bold and Italic
#'
#' Add or remove bold and italic styling to a cell in a table.
#'
#' @inheritParams modify_footnote2
#'
#' @return Updated gtsummary object
#' @name modify_bold_italic
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' tbl <- trial |>
#'   tbl_summary(include = grade) |>
#'   modify_bold(columns = label, rows = row_type == "label") |>
#'   modify_italic(columns = label, rows = row_type == "level")
#' tbl
#'
#' # Example 2 ----------------------------------
#' tbl |>
#'   remove_bold(columns = label, rows = row_type == "label") |>
#'   remove_italic(columns = label, rows = row_type == "level")
NULL

#' @rdname modify_bold_italic
#' @export
modify_bold <- function(x, columns, rows) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(columns)
  check_not_missing(rows)
  check_class(x, "gtsummary")
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  x$call_list <- c(x$call_list, list(modify_bold = match.call()))
  .modify_text_format(x = x, columns = columns, rows = {{ rows }}, text_format = "bold", undo = FALSE)
}

#' @rdname modify_bold_italic
#' @export
remove_bold <- function(x, columns, rows) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(columns)
  check_not_missing(rows)
  check_class(x, "gtsummary")
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  x$call_list <- c(x$call_list, list(remove_bold = match.call()))
  .modify_text_format(x = x, columns = columns, rows = {{ rows }}, text_format = "bold", undo = TRUE)
}

#' @rdname modify_bold_italic
#' @export
modify_italic <- function(x, columns, rows) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(columns)
  check_not_missing(rows)
  check_class(x, "gtsummary")
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  x$call_list <- c(x$call_list, list(modify_italic = match.call()))
  .modify_text_format(x = x, columns = columns, rows = {{ rows }}, text_format = "italic", undo = FALSE)
}

#' @rdname modify_bold_italic
#' @export
remove_italic <- function(x, columns, rows) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(columns)
  check_not_missing(rows)
  check_class(x, "gtsummary")
  .check_rows_input(x, {{ rows }})

  # process columns ------------------------------------------------------------
  cards::process_selectors(
    scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  x$call_list <- c(x$call_list, list(remove_italic = match.call()))
  .modify_text_format(x = x, columns = columns, rows = {{ rows }}, text_format = "italic", undo = TRUE)
}

.modify_text_format <- function(x, columns, rows, text_format, undo = FALSE) {
  # add updates to `x$table_styling$text_format` -------------------------------
  x$table_styling$text_format <-
    dplyr::bind_rows(
      x$table_styling$text_format,
      tidyr::expand_grid(
        column = columns,
        rows = list(enquo(rows)),
        format_type = text_format,
        undo_text_format = undo
      )
    )

  # return table ---------------------------------------------------------------
  x
}
