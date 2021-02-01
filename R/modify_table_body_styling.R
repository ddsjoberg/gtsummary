#' Modify table_styling
#'
#' This is a function meant for advanced users to gain
#' more control over the characteristics of the resulting
#' gtsummary table.
#'
#' Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary definition}
#' vignette for information on `.$table_styling` objects.
#'
#' @param x gtsummary object
#' @param columns vector or selector of columns in `x$table_body`
#' @param rows predicate string expression to select rows. `rows = NA` often indicates all rows.
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
  if (!quo_is_null(rows) && !is.logical(eval_tidy(rows, data = x$table_body))) {
    abort("The `rows=` predicate expression must result in logical vector when evaluated with `x$table_body`")
  }
  # converting rows expression to string
  rows <- quo_text(rows, width = 500L) %>% {switch(. != "NULL", .)}


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
          rows = rows %||% NA_character_,
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
          rows = rows %||% NA_character_,
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
          rows = rows %||% NA_character_,
          fmt_fun = fmt_fun
        )
      )
  }

  # text_format ----------------------------------------------------------------
  if (!is.null(text_format)) {
    x$table_styling$text_format <-
      list(
        column = columns,
        rows = rows,
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
        rows = rows,
        symbol = missing_symbol
      ) %>%
      purrr::cross_df() %>%
      {bind_rows(x$table_styling$fmt_missing, .)}
  }

  # return x -------------------------------------------------------------------
  x
}


