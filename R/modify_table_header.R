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
#'
#' @seealso `modify_table_body()`
#' @seealso See \href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary internals vignette}
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

  # update table_header --------------------------------------------------------
  x$table_header <- table_header_fill_missing(x$table_header, x$table_body)

  # convert column input to string ---------------------------------------------
  column <-
    .select_to_varnames(
      select = {{ column }},
      var_info = x$table_header$column,
      arg_name = "column"
    )

  # if no columns selected, returning unaltered
  if (is.null(column)) return(x)

  .convert_call_to_table_styling(
    x = x, column = column, label = label, hide = hide, align = align,
    missing_emdash = missing_emdash, indent = indent,
    text_interpret = text_interpret, bold = bold, italic = italic,
    fmt_fun = fmt_fun, footnote_abbrev = footnote_abbrev,
    footnote = footnote, spanning_header = spanning_header
  )

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

.convert_call_to_table_styling <- function(x, column, label, hide, align,
                                                missing_emdash, indent,
                                                text_interpret, bold, italic,
                                                fmt_fun, footnote_abbrev,
                                                footnote, spanning_header) {
  call_list <- list()

  if (!is.null(label)) {
    if (!is.null(text_interpret))
      call_list[["label"]] <- expr(modify_table_styling(columns = !!column, label = !!label, text_interpret = !!text_interpret))
    else
      call_list[["label"]] <- expr(modify_table_styling(columns = !!column, label = !!label))
  }
  if (!is.null(spanning_header)) {
    call_list[["spanning_header"]] <- expr(modify_table_styling(columns = !!column, label = !!spanning_header))
  }
  if (!is.null(hide)) {
    call_list[["hide"]] <- expr(modify_table_styling(columns = !!column, hide = !!hide))
  }
  if (!is.null(hide)) {
    call_list[["align"]] <- expr(modify_table_styling(columns = !!column, hide = !!align))
  }
  if (!is.null(missing_emdash)) {
    call_list[["missing_emdash"]] <- expr(modify_table_styling(columns = !!column, rows = !!missing_emdash, missing_symbol = "---"))
  }
  if (!is.null(indent)) {
    call_list[["indent"]] <- expr(modify_table_styling(columns = !!column, rows = !!indent, text_format = "indent"))
  }
  if (!is.null(bold)) {
    call_list[["bold"]] <- expr(modify_table_styling(columns = !!column, rows = !!bold, text_format = "bold"))
  }
  if (!is.null(italic)) {
    call_list[["italic"]] <- expr(modify_table_styling(columns = !!column, rows = !!italic, text_format = "italic"))
  }
  if (!is.null(fmt_fun)) {
    call_list[["fmt_fun"]] <- expr(modify_table_styling(columns = !!column, rows = NA, fmt_fun = fmt_fun))
  }
  if (!is.null(footnote_abbrev)) {
    call_list[["footnote_abbrev"]] <- expr(modify_table_styling(columns = !!column, rows = NA, footnote_abbrev = !!footnote_abbrev))
  }
  if (!is.null(footnote)) {
    call_list[["footnote"]] <- expr(modify_table_styling(columns = !!column, rows = NA, footnote = !!footnote))
  }

  lifecycle::deprecate_warn("1.4.0",
                            "gtsummary::modify_table_header()",
                            "modify_table_styling()")
  ui_info("Using {ui_code('modify_table_styling()')} instead of {ui_code('modify_table_header()')}")
  usethis::ui_todo("Replace your code with the following:")
  c(list(expr(x)), call_list) %>%
    map(deparse) %>%
    unlist() %>%
    paste(collapse = " %>% ") %>%
    ui_code() %>%
    usethis::ui_todo()

  # c(list(expr(x)), call_list) %>%
  #   # concatenating expressions with %>% between each of them
  #   reduce(function(x, y) expr(!!x %>% !!y)) %>%
  #   # evaluating expressions
  #   eval()
}

