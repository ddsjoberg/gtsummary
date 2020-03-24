#' Modify column headers in gtsummary tables
#'
#' Column labels can be modified to include calculated statistics;
#' e.g. the N can be dynamically included by wrapping it in curly brackets
#' (following [glue::glue] syntax).
#'
#' @param x gtsummary object, e.g. `tbl_summary` or `tbl_regression`
#' @param stat_by String specifying text to include above the summary statistics
#' stratified by a variable.  Only use with stratified `tbl_summary` objects.
#' The following fields are available for use in the
#' headers:
#' * `{n}` number of observations in each group,
#' * `{N}` total number of observations,
#' * `{p}` percentage in each group,
#' * `{level}` the 'by' variable level,
#' * `"fisher.test"` for a Fisher's exact test,
#'
#' Syntax follows [glue::glue],
#' e.g. `stat_by = "**{level}**, N = {n} ({style_percent(p)\%})"`.
#' The `by` argument from the parent `tbl_summary()` cannot be `NULL`.
#' @param ... Specifies column label of any other column in `.$table_body`.
#' Argument is the column name, and the value is the new column header
#' (e.g. `p.value = "Model P-values"`). Use
#' `print(x$table_body)` to see columns available.
#' @param text_interpret indicates whether text will be interpreted as markdown (`"md"`)
#' or HTML (`"html"`).  The text is interpreted with the {gt} package's `md()` or
#' `html()` functions.  The default is `"md"`, and is ignored when the print engine
#' is not {gt}.
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_survival tools
#' @author Daniel D. Sjoberg
#' @examples
#' tbl_col_ex1 <-
#'   trial[c("age", "grade", "response")] %>%
#'   tbl_summary() %>%
#'   modify_header(stat_0 = "**All Patients**, N = {N}")
#'
#' tbl_col_ex2 <-
#'   trial[c("age", "grade", "response", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   modify_header(
#'     stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})"
#'   )
#' @return Function return the same class of gtsummary object supplied
#' @export
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_col_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_col_ex2.png}{options: width=50\%}}

modify_header <- function(x, stat_by = NULL, ..., text_interpret = c("md", "html")) {

  # converting the passed ... to a list, OR if nothing passed to NULL
  if (length(list(...)) == 0) {
    passed_dots <- NULL
  } else {
    passed_dots <- list(...)
  }

  do.call(
    modify_header_internal,
    c(list(
      x = x, stat_by = stat_by, text_interpret = text_interpret,
      .save_call = TRUE
    ), passed_dots)
  )
}

