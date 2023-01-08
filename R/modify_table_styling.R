#' Modify Table Styling
#'
#' This is a function meant for advanced users to gain
#' more control over the characteristics of the resulting
#' gtsummary table by directly modifying `.$table_styling`
#'
#' Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary definition}
#' vignette for information on `.$table_styling` objects.
#'
#' @param x gtsummary object
#' @param columns vector or selector of columns in `x$table_body`
#' @param rows predicate expression to select rows in `x$table_body`.
#' Can be used to style footnote, formatting functions, missing symbols,
#' and text formatting. Default is `NULL`. See details below.
#' @param label string of column label(s)
#' @param hide logical indicating whether to hide column from output
#' @param align string indicating alignment of column, must be one of
#' `c("left", "right", "center")`
#' @param text_format string indicated which type of text formatting to apply to the rows and columns.
#' Must be one of `c("bold", "italic", "indent", "indent2")`. Do not assign
#' both `"indent"` and `"indent2"` to the same cell.
#' @param text_interpret string, must be one of `"md"` or `"html"`
#' @param fmt_fun function that formats the statistics in the
#' columns/rows in `columns=` and `rows=`
#' @param footnote_abbrev string with abbreviation definition, e.g.
#' `"CI = Confidence Interval"`
#' @param footnote string with text for footnote
#' @param spanning_header string with text for spanning header
#' @param missing_symbol string indicating how missing values are formatted.
#' @param undo_text_format rarely used. Logical that undoes the indent, bold,
#' and italic styling when `TRUE`
#' @param cols_merge_pattern \lifecycle{experimental} glue-syntax string
#' indicating how to merge
#' columns in `x$table_body`. For example, to construct a confidence interval
#' use `"{conf.low}, {conf.high}"`. The first column listed in the pattern
#' string must match the single column name passed in `columns=`.
#'
#' @seealso `modify_table_body()`
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary internals vignette}
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @export
#' @family Advanced modifiers
#'
#' @section rows argument:
#' The rows argument accepts a predicate expression that is used to specify
#' rows to apply formatting. The expression must evaluate to a logical when
#' evaluated in `x$table_body`. For example, to apply formatting to the age rows
#' pass `rows = variable == "age"`. A vector of row numbers is NOT acceptable.
#'
#' A couple of things to note when using the `rows=` argument.
#' 1. You can use saved objects to create the predicate argument, e.g.
#' `rows = variable == letters[1]`.
#' 2. The saved object cannot share a name with a column in `x$table_body`.
#' The reason for this is that in `tbl_merge()` the columns are renamed,
#' and the renaming process cannot disambiguate the `variable` column from
#' an external object named `variable` in the following expression
#' `rows = .data$variable = .env$variable`.
#'
#' @section cols_merge_pattern argument:
#'
#' There are planned updates to the implementation of column merging.
#' Currently, this function replaces the numeric column with a
#' formatted character column following `cols_merge_pattern=`.
#' Once `gt::cols_merge()` gains the `rows=` argument the
#' implementation will be updated to use it, which will keep
#' numeric columns numeric. For the _vast majority_ of users,
#' _the planned change will be go unnoticed_.
#'
#' If this functionality is used in conjunction with `tbl_stack()` (which
#' includes `tbl_uvregression()`), there is potential issue with printing.
#' When columns are stack AND when the column-merging is
#' defined with a quosure, you may run into issues due to the loss of the
#' environment when 2 or more quosures are combined. If the expression
#' version of the quosure is the same as the quosure (i.e. no evaluated
#' objects), there should be no issues. Regardless, this argument is used
#' internally with care, and it is _not_ recommended for users.


modify_table_styling <- function(x,
                                 columns,
                                 rows = NULL,
                                 label = NULL,
                                 spanning_header = NULL,
                                 hide = NULL,
                                 footnote = NULL,
                                 footnote_abbrev = NULL,
                                 align = NULL,
                                 missing_symbol = NULL,
                                 fmt_fun = NULL,
                                 text_format = NULL,
                                 undo_text_format = FALSE,
                                 text_interpret = c("md", "html"),
                                 cols_merge_pattern = NULL) {
  updated_call_list <- c(x$call_list, list(modify_table_styling = match.call()))
  # checking inputs ------------------------------------------------------------
  .assert_class(x, "gtsummary")

  text_interpret <- match.arg(text_interpret) %>%
    {
      paste0("gt::", .)
    }
  if (!is.null(text_format)) {
    text_format <- match.arg(text_format,
      choices = c("bold", "italic", "indent", "indent2"),
      several.ok = TRUE
    )
  }
  rows <- enquo(rows)
  rows_eval_error <-
    tryCatch(
      eval_tidy(rows, data = x$table_body) %>%
        {
          !is.null(.) && !is.logical(.)
        },
      error = function(e) TRUE
    )
  if (rows_eval_error) {
    abort("The `rows=` predicate expression must result in logical vector when evaluated with `x$table_body`")
  }

  # update table_styling -------------------------------------------------------
  x <- .update_table_styling(x)

  # convert column input to string ---------------------------------------------
  columns <-
    .select_to_varnames(
      select = {{ columns }},
      data = x$table_body,
      arg_name = "columns"
    )

  # if no columns selected, returning unaltered
  if (is.null(columns)) {
    return(x)
  }

  # label ----------------------------------------------------------------------
  if (!is.null(label)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, interpret_label = text_interpret, label = label),
        by = "column"
      )
  }

  # spanning_header ------------------------------------------------------------
  if (!is.null(spanning_header)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, interpret_label = text_interpret, spanning_header = spanning_header),
        by = "column"
      )
  }

  # hide -----------------------------------------------------------------------
  if (!is.null(hide)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, hide = hide),
        by = "column"
      )
  }

  # align ----------------------------------------------------------------------
  if (!is.null(align)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, align = align),
        by = "column"
      )
  }

  # footnote -------------------------------------------------------------------
  if (!is.null(footnote)) {
    x$table_styling$footnote <-
      bind_rows(
        x$table_styling$footnote,
        tibble(
          column = columns,
          rows = list(rows),
          text_interpret = text_interpret,
          footnote = footnote
        )
      )
  }

  # footnote_abbrev ------------------------------------------------------------
  if (!is.null(footnote_abbrev)) {
    x$table_styling$footnote_abbrev <-
      bind_rows(
        x$table_styling$footnote_abbrev,
        tibble(
          column = columns,
          rows = list(rows),
          text_interpret = text_interpret,
          footnote = footnote_abbrev
        )
      )
  }

  # fmt_fun --------------------------------------------------------------------
  if (!is.null(fmt_fun)) {
    if (rlang::is_function(fmt_fun)) fmt_fun <- list(fmt_fun)
    x$table_styling$fmt_fun <-
      bind_rows(
        x$table_styling$fmt_fun,
        tibble(
          column = columns,
          rows = list(rows),
          fmt_fun = fmt_fun
        )
      )
  }

  # text_format ----------------------------------------------------------------
  if (!is.null(text_format)) {
    x$table_styling$text_format <-
      list(
        column = columns,
        rows = list(rows),
        format_type = text_format,
        undo_text_format = undo_text_format
      ) %>%
      {
        tidyr::expand_grid(!!!.)
      } %>%
      {
        bind_rows(x$table_styling$text_format, .)
      }
  }

  # missing_symbol -------------------------------------------------------------
  if (!is.null(missing_symbol)) {
    x$table_styling$fmt_missing <-
      list(
        column = columns,
        rows = list(rows),
        symbol = missing_symbol
      ) %>%
      {
        tidyr::expand_grid(!!!.)
      } %>%
      {
        bind_rows(x$table_styling$fmt_missing, .)
      }
  }

  # cols_merge_pattern ---------------------------------------------------------
  if (!is.null(cols_merge_pattern)) {
    x <-
      .modify_cols_merge(
        x,
        column = columns,
        rows = !!rows,
        pattern = cols_merge_pattern
      )
  }

  # return x -------------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}


# function to add merging columns instructions
.modify_cols_merge <- function(x, column, rows, pattern) {
  rows <- enquo(rows)
  all_columns <-
    str_extract_all(pattern, "\\{.*?\\}") %>%
    map(~ str_remove_all(.x, pattern = fixed("}"))) %>%
    map(~ str_remove_all(.x, pattern = fixed("{"))) %>%
    unlist()

  if (!is.na(pattern) && !all(all_columns %in% x$table_styling$header$column)) {
    paste(
      "All columns specified in `cols_merge_pattern=`",
      "must be present in `x$table_body`"
    ) %>%
      abort()
  }

  if (!is.na(pattern) && !identical(column, all_columns[1])) {
    paste(
      "A single column must be passed when using `cols_merge_pattern=`,",
      "and that column must be the first to appear in the pattern argument."
    ) %>%
      abort()
  }

  x$table_styling$cols_merge <-
    x$table_styling$cols_merge %>%
    # remove previous merging for specified column
    filter(!.data$column %in% .env$column) %>%
    # append new merge instructions
    bind_rows(
      tibble(
        column = column,
        rows = list(rows),
        pattern = pattern
      )
    )

  # hiding all but the first column
  x <- modify_column_hide(x, columns = all_columns[-1])

  # return gtsummary table
  x
}
