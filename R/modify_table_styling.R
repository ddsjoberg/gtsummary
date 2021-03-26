#' Modify Table Styling
#'
#' This is a function meant for advanced users to gain
#' more control over the characteristics of the resulting
#' gtsummary table by directly modifying `.$table_styling`
#'
#' Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary definition}
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
#' Must be one of `c("bold", "italic", "indent")`
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
#'
#' @seealso `modify_table_body()`
#' @seealso See \href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary internals vignette}
#' @export
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

modify_table_styling <- function(x,columns,
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
                                 text_interpret = c("md", "html")
                                 ) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) stop("`x=` must be class 'gtsummary'", call. = FALSE)
  if (is.null(x$table_styling)) x <- .convert_table_header_to_styling(x)
  text_interpret <- match.arg(text_interpret) %>% {paste0("gt::", .)}
  if (!is.null(text_format))
    text_format <- match.arg(text_format,
                             choices = c("bold", "italic", "indent"),
                             several.ok = TRUE)
  rows <- enquo(rows)
  rows_eval_error <-
    tryCatch(
      eval_tidy(rows, data = x$table_body) %>%
      {!is.null(.) && !is.logical(.)},
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
  if (is.null(columns)) return(x)

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
      purrr::cross_df() %>%
      {bind_rows(x$table_styling$text_format, .)}
  }

  # missing_symbol -------------------------------------------------------------
  if (!is.null(missing_symbol)) {
    x$table_styling$fmt_missing <-
      list(
        column = columns,
        rows = list(rows),
        symbol = missing_symbol
      ) %>%
      purrr::cross_df() %>%
      {bind_rows(x$table_styling$fmt_missing, .)}
  }

  # return x -------------------------------------------------------------------
  x
}


# this is an experimental function to merge columns
modify_cols_merge <- function(x, rows, pattern) {
  rows <- enquo(rows)
  column <-
    str_extract_all(pattern, "\\{.*?\\}") %>%
    map(str_remove_all, pattern = fixed("}")) %>%
    map(str_remove_all, pattern = fixed("{")) %>%
    unlist() %>%
    purrr::pluck(1)

  x$table_styling$cols_merge <-
    x$table_styling$cols_merge %>%
    # remove previous merging for specified column
    filter(!.data$column %in% .env$column) %>%
    # append new merge instructions
    bind_rows(
      tibble(column = column,
             rows = list(row),
             pattern = pattern)
    )

  # return gtsummary table
  x
}
