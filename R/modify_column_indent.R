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
#'   modify_column_indent(columns = label, indentation = 0L)
modify_column_indent <- function(x, columns, rows = NULL, indentation = 4L,
                                 double_indent = FALSE, undo = FALSE) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_table_styling = match.call()))

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_not_missing(x)
  check_not_missing(columns)
  if (!is_integerish(indentation) || indentation < 0L) {
    cli::cli_abort(
      "The {.arg indentation} argument must be a non-negative integer.",
      call = get_cli_abort_call()
    )
  }

  # if deprecated arguments passed ---------------------------------------------
  if (!missing(double_indent) || !missing(undo)) {
    if (!missing(double_indent)) {
      lifecycle::deprecate_warn(
        when = "2.0.0",
        what = I("modify_column_indent(double_indent=TRUE)"),
        with = I("modify_column_indent(indentation=8)")
      )
    }
    if (!missing(undo)) {
      lifecycle::deprecate_warn(
        when = "2.0.0",
        what = I("modify_column_indent(undo=TRUE)"),
        with = I("modify_column_indent(indentation=0)")
      )
    }

    x <-
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
        indentation = indentation
      )
  }

  # return x -------------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
