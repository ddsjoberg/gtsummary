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
  updated_call_list <- c(x$call_list, list(modify_table_header = match.call()))
  # checking inputs ------------------------------------------------------------
  lifecycle::deprecate_warn(
    "1.4.0", "gtsummary::modify_table_header()", "modify_table_styling()"
  )
  if (!inherits(x, "gtsummary")) stop("`x=` must be class 'gtsummary'", call. = FALSE)

  # convert column input to string ---------------------------------------------
  column <-
    .select_to_varnames(
      select = {{ column }},
      var_info = x$table_header$column,
      arg_name = "column"
    )

  # Updating gtsummary internals
  if (is.null(x$table_styling)) x <- .convert_table_header_to_styling(x)

  # if no columns selected, returning unaltered
  if (is.null(column)) {
    return(x)
  }

  x <-
    .convert_header_call_to_styling_call(
      x = x, column = column, label = label, hide = hide, align = align,
      missing_emdash = missing_emdash, indent = indent,
      text_interpret = text_interpret, bold = bold, italic = italic,
      fmt_fun = fmt_fun, footnote_abbrev = footnote_abbrev,
      footnote = footnote, spanning_header = spanning_header,
      call = match.call(), env = rlang::caller_env()
    )

  x$call_list <- updated_call_list
  x
}

.convert_header_call_to_styling_call <- function(x, column, label, hide, align,
                                                 missing_emdash, indent,
                                                 text_interpret, bold, italic,
                                                 fmt_fun, footnote_abbrev,
                                                 footnote, spanning_header,
                                                 call, env) {
  styling_call_list <- list()
  call_list <- as.list(call)[-1]
  names(call_list)[which(names(call_list) %in% "column")] <- "columns"

  # header formatting ----------------------------------------------------------
  header_args <- c(
    "columns", "label", "hide", "align", "text_interpret",
    "spanning_header", "footnote", "footnote_abbrev", "fmt_fun"
  )
  header_call_list <- call_list[names(call_list) %in% header_args]
  if (length(header_call_list) > 1) {
    styling_call_list[["header"]] <- expr(modify_table_styling(!!!header_call_list))
  }

  # row formatting -------------------------------------------------------------
  if ("bold" %in% names(call_list)) {
    styling_call_list[["bold"]] <-
      expr(modify_table_styling(
        columns = !!call_list$columns,
        rows = !!parse_expr(call_list$bold),
        text_format = "bold"
      ))
  }
  if ("italic" %in% names(call_list)) {
    styling_call_list[["italic"]] <-
      expr(modify_table_styling(
        columns = !!call_list$columns,
        rows = !!parse_expr(call_list$italic),
        text_format = "italic"
      ))
  }
  if ("indent" %in% names(call_list)) {
    styling_call_list[["indent"]] <-
      expr(modify_table_styling(
        columns = !!call_list$columns,
        rows = !!parse_expr(call_list$indent),
        text_format = "indent"
      ))
  }
  if ("missing_emdash" %in% names(call_list)) {
    styling_call_list[["missing_emdash"]] <-
      expr(modify_table_styling(
        columns = !!call_list$columns,
        rows = !!parse_expr(call_list$missing_emdash),
        missing_symbol = "\U2014"
      ))
  }

  # printing code to use instead of `modify_table_header()`
  cli_alert_info("Use {.code modify_table_styling()} instead of {.code modify_table_header()}")
  cli_ul("Replace the {.code modify_table_header()} call with the following.")
  c(list(expr(x)), styling_call_list) %>%
    map(~ deparse(.) %>%
      paste(collapse = "") %>%
      stringr::str_squish()) %>%
    unlist() %>%
    paste(collapse = " %>%\n  ") %>%
    cat()

  # evaluating `modify_table_header()` code
  c(list(expr(!!x)), styling_call_list) %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval_tidy(env = env)
}
