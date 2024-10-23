#' Modify column indentation
#'
#' Add, increase, or reduce indentation for columns.
#'
#' @inheritParams modify_table_styling
#' @param double_indent,undo `r lifecycle::badge("deprecated")`
#'
#' @return a gtsummary table
#' @export
#'
#' @family Advanced modifiers
#' @examples
#' # remove indentation from `tbl_summary()`
#' trial |>
#'   tbl_summary(include = grade) |>
#'   modify_column_indent(columns = label, indent = 0L)
#'
#' # increase indentation in `tbl_summary`
#' trial |>
#'   tbl_summary(include = grade) |>
#'   modify_column_indent(columns = label, rows = !row_type %in% 'label', indent = 8L)
modify_column_indent <- function(x, columns, rows = NULL, indent = 4L,
                                 double_indent, undo) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_table_styling = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_not_missing(x)
  check_not_missing(columns)

  # if deprecated arguments passed ---------------------------------------------
  if (!missing(double_indent) || !missing(undo)) {
    check_pkg_installed("withr")
    if (!missing(double_indent)) {
      lifecycle::deprecate_warn(
        when = "2.0.0",
        what = I(glue("modify_column_indent(double_indent={double_indent})")),
        with = I(glue("modify_column_indent(indent={ifelse(double_indent, 8, 4)})"))
      )
    } else double_indent <- FALSE
    if (!missing(undo) && isTRUE(undo)) {
      lifecycle::deprecate_warn(
        when = "2.0.0",
        what = I(glue("modify_column_indent(undo=TRUE)")),
        with = I("modify_column_indent(indent=0)")
      )
    }
    else if (!missing(undo) && isFALSE(undo)) {
      lifecycle::deprecate_warn(
        when = "2.0.0",
        what = I(glue("modify_column_indent(undo)")),
        details = "Argument has been ignored."
      )
    } else undo <- FALSE

    x <-
      # run without triggering additional deprecation messaging
      withr::with_options(
       new = list(lifecycle_verbosity = "quiet"),
       code =
         modify_table_styling(
           x,
           columns = {{ columns }},
           rows = {{ rows }},
           text_format = ifelse(double_indent, "indent2", "indent"),
           undo_text_format = undo
         )
       )
  }
  # otherwise, continue with the non-deprecated arguments ----------------------
  else {
    x <-
      modify_table_styling(
        x,
        columns = {{ columns }},
        rows = {{ rows }},
        indent = indent
      )
  }

  # return x -------------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
