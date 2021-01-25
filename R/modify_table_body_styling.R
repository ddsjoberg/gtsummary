#' Modify table_body_styling
#'
#' This is a function meant for advanced users to gain
#' more control over the characteristics of the resulting
#' gtsummary table.
#'
#' Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary definition}
#' vignette for information on `.$table_header` objects.
#'
#' @param x gtsummary object
#' @param columns vector or selector of columns in `x$table_body`
#' @param rows predicate string expression to select rows. `rows = NA` often indicates all rows.
#' @param label string of column label(s)
#' @param hide logical indicating whether to hide column from output
#' @param align string indicating alignment of column, must be one of
#' `c("left", "right", "center")`
#' @param indent logical indicating if the rows selected in `rows=` should be indented
#' @param bold logical indicating if the rows selected in `rows=` should be bold
#' @param italic logical indicating if the rows selected in `rows=` should be italic
#' @param text_interpret string, must be one of `"gt::md"` or `"gt::html"`
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

modify_table_styling <- function(x,columns, rows = NA_character_,
                                 label = NULL,
                                 spanning_header = NULL,
                                 hide = NULL,
                                 align = NULL,
                                 footnote = NULL,
                                 footnote_abbrev = NULL,
                                 missing_symbol = NULL,
                                 fmt_fun = NULL,
                                 indent = FALSE,
                                 bold = FALSE,
                                 italic = FALSE,
                                 undo_text_format = FALSE,
                                 text_interpret = c("gt::md", "gt::html")
                                 ) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) stop("`x=` must be class 'gtsummary'", call. = FALSE)
  if (is.null(x$table_body_styling)) x <- .convert_table_header_to_styling(x)
  text_interpret <- match.arg(text_interpret)

  # update table_header --------------------------------------------------------
  x <- .update_table_body_styling(x)

  # convert column input to string ---------------------------------------------
  columns <-
    .select_to_varnames(
      select = {{ columns }},
      var_info = x$table_header$column,
      arg_name = "column"
    )

  # if no columns selected, returning unaltered
  if (is.null(columns)) return(x)

  # label ----------------------------------------------------------------------
  if (!is.null(label)) {
    x$table_body_styling$header <-
      x$table_body_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, interpret_label = text_interpret, label = label),
        by = "column"
      )
  }

  # spanning_header ------------------------------------------------------------
  if (!is.null(spanning_header)) {
    x$table_body_styling$header <-
      x$table_body_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, interpret_label = text_interpret, spanning_header = spanning_header),
        by = "column"
      )
  }

  # hide -----------------------------------------------------------------------
  if (!is.null(hide)) {
    x$table_body_styling$header <-
      x$table_body_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, hide = hide),
        by = "column"
      )
  }

  # align ----------------------------------------------------------------------
  if (!is.null(align)) {
    x$table_body_styling$header <-
      x$table_body_styling$header %>%
      dplyr::rows_update(
        tibble(column = columns, align = align),
        by = "column"
      )
  }

  # footnote -------------------------------------------------------------------
  if (!is.null(footnote)) {
    x$table_body_styling$footnote <-
      bind_rows(
        x$table_body_styling$footnote,
        tibble(
          column = columns,
          rows = rows,
          text_interpret = text_interpret,
          footnote = footnote
        )
      )
  }

  # footnote_abbrev ------------------------------------------------------------
  if (!is.null(footnote_abbrev)) {
    x$table_body_styling$footnote_abbrev <-
      bind_rows(
        x$table_body_styling$footnote_abbrev,
        tibble(
          column = columns,
          rows = rows,
          text_interpret = text_interpret,
          footnote_abbrev = footnote_abbrev
        )
      )
  }

  # fmt_fun --------------------------------------------------------------------
  if (!is.null(fmt_fun)) {
    if (rlang::is_function(fmt_fun)) fmt_fun <- list(fmt_fun)
    x$table_body_styling$fmt_fun <-
      bind_rows(
        x$table_body_styling$fmt_fun,
        tibble(
          column = columns,
          rows = rows,
          fmt_fun = fmt_fun
        )
      )
  }

  # text_format ----------------------------------------------------------------
  x$table_body_styling$text_format <-
    list(
      column = columns,
      rows = rows,
      format_type =
        list(bold = bold, italic = italic, indent = indent) %>%
        purrr::imap(~switch(.x, .y)) %>%
        unlist(),
      undo_text_format = undo_text_format
    ) %>%
    purrr::cross_df() %>%
    {bind_rows(x$table_body_styling$text_format, .)}

  # return x -------------------------------------------------------------------
  x
}

# this fn updates `table_body_styling` list to match `table_body`
.update_table_body_styling <- function(x) {

  x$table_body_styling$header <-
    tibble(
      column = names(x$table_body),
      hide = TRUE,
      align = "center",
      interpret_label = "gt::md",
      label = names(x$table_body),
      interpret_spanning_header = "gt::md",
      spanning_header = NA_character_
    ) %>%
    dplyr::rows_update(
      x$table_body_styling$header,
      by = "column"
    )

  x
}
