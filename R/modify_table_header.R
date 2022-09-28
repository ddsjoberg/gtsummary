#' Modify table_header
#'
#' \lifecycle{deprecated}
#' Use `modify_table_styling()` instead.
#'
#' @param x gtsummary object
#' @param column vector or selector of columns in `x$table_body`
#' @param label string of column label
#' @param hide logical indicating whether to hide column from output
#' @param align string indicating alignment of column, must be one of
#' `c("left", "right", "center")`
#' @param missing_emdash string that evaluates to logical identifying rows to
#' include em-dash for missing values, e.g. `"reference_row == TRUE"`
#' @param indent string that evaluates to logical identifying rows to indent
#' @param bold string that evaluates to logical identifying rows to bold
#' @param italic string that evaluates to logical identifying rows to italicize
#' @param text_interpret string, must be one of `"gt::md"` or `"gt::html"`
#' @param fmt_fun function that formats the statistics in the column
#' @param footnote_abbrev string with abbreviation definition, e.g.
#' `"CI = Confidence Interval"`
#' @param footnote string with text for column footnote
#' @param spanning_header string with text for spanning header
#' @export
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @keywords internal

modify_table_header <- function(x, column, label = NULL, hide = NULL, align = NULL,
                                missing_emdash = NULL, indent = NULL,
                                text_interpret = NULL, bold = NULL, italic = NULL,
                                fmt_fun = NULL, footnote_abbrev = NULL,
                                footnote = NULL, spanning_header = NULL) {
  # checking inputs ------------------------------------------------------------
  lifecycle::deprecate_stop(
    "1.4.0", "gtsummary::modify_table_header()", "modify_table_styling()"
  )
}
