#' Modify table_header
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
#' @param column columns to update
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
#'
#' @return gtsummary object
#' @export
#'
#'
#' @examples
#' # Example 1 ----------------------------------
#' modify_table_header_ex1 <-
#'   lm(mpg ~ factor(cyl), mtcars) %>%
#'   tbl_regression() %>%
#'   modify_table_header(column = estimate,
#'                       label = "**Coefficient**",
#'                       fmt_fun = function(x) style_sigfig(x, digits = 5),
#'                       footnote = "Regression Coefficient") %>%
#'   modify_table_header(column = "p.value",
#'                       hide = TRUE)
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_table_header_ex1.png}{options: width=50\%}}

modify_table_header <- function(x, column, label = NULL, hide = NULL, align = NULL,
                                missing_emdash = NULL, indent = NULL,
                                text_interpret = NULL, bold = NULL, italic = NULL,
                                fmt_fun = NULL, footnote_abbrev = NULL,
                                footnote = NULL, spanning_header = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) stop("`x=` must be class 'gtsummary'", call. = FALSE)

  # convert column input to string ---------------------------------------------
  column <-
    .select_to_varnames(
      select = {{ column }},
      var_info = x$table_header$column,
      arg_name = "column"
    )

  # if no columns selected, returning unaltered
  if (is.null(column)) return(x)

  # label ----------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "label", update = label,
    class_check = "is.character", in_check = NULL
  )

  # hide -----------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "hide", update = hide,
    class_check = "is.logical", in_check = NULL
  )

  # align ----------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "align", update = align,
    class_check = "is.character", in_check = c("left", "right", "center")
  )

  # missing_emdash -------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "missing_emdash", update = missing_emdash,
    class_check = "is.character", in_check = NULL
  )

  # indent ---------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "indent", update = indent,
    class_check = "is.character", in_check = NULL
  )

  # text_interpret -------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "text_interpret", update = text_interpret,
    class_check = "is.character", in_check = c("gt::md", "gt::html")
  )

  # bold -----------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "bold", update = bold,
    class_check = "is.character", in_check = NULL
  )

  # italic ---------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "italic", update = italic,
    class_check = "is.character", in_check = NULL
  )

  # fmt_fun --------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "fmt_fun", update = fmt_fun,
    class_check = "is.function", in_check = NULL, in_list = TRUE
  )

  # footnote_abbrev ------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "footnote_abbrev", update = footnote_abbrev,
    class_check = "is.character", in_check = NULL
  )

  # footnote -------------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "footnote", update = footnote,
    class_check = "is.character", in_check = NULL
  )

  # spanning_header ------------------------------------------------------------
  x <- .update_table_header_element(
    x = x, column = column, element = "spanning_header", update = spanning_header,
    class_check = "is.character", in_check = NULL
  )

  # return gtsummary object ----------------------------------------------------
  x
}

.update_table_header_element <- function(x, column, element, update,
                                         class_check = NULL, in_check = NULL,
                                         in_list = FALSE) {
  # return unaltered if no change requested ------------------------------------
  if (is.null(update)) return(x)

  # checking inputs ------------------------------------------------------------
  if (length(update) != 1) {
    glue("`{element}=` argument must be of length one.") %>%
      abort()
  }
  if (!is.null(class_check) && !do.call(class_check, list(update))) {
    glue("`{element}=` argument must satisfy `{class_check}()`") %>%
      abort()
  }
  if (!is.null(in_check) && !update %in% in_check) {
    glue("`{element}=` argument must be one of {paste(in_check, collapse = ", ")}") %>%
      abort()
  }

  # making update --------------------------------------------------------------
  if (in_list) update <- list(update)
  x$table_header <-
    x$table_header %>%
    dplyr::rows_update(
      tibble(column = column, element = update) %>% set_names(c("column", element)),
      by = "column"
    )

  # return gtsummary object ----------------------------------------------------
  x
}
