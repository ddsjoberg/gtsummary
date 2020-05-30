#' Modify column headers in gtsummary tables
#'
#' Column labels can be modified to include calculated statistics;
#' e.g. the N can be dynamically included by wrapping it in curly brackets
#' (following [glue::glue] syntax).
#'
#' @param x gtsummary object, e.g. `tbl_summary` or `tbl_regression`
#' @param update list of formulas or a single formula specifying the updated
#' column label. Columns from `x$table_body` may be selected.
#' @param stat_by Used with `tbl_summary(by=)` objects with a `by=` argument.
#' String specifying text to include above the summary statistics.
#' The following fields are available for use in the
#' headers:
#' * `{n}` number of observations in each group,
#' * `{N}` total number of observations,
#' * `{p}` percentage in each group,
#' * `{level}` the 'by' variable level,
#'
#' Syntax follows [glue::glue()],
#' e.g. `stat_by = "**{level}**, N = {n} ({style_percent(p)}%)"`.
#' @param ... Specify a column and updated column label,
#' e.g. `modify_header(p.value = "Model P-values")`. This is provided as an alternative to the
#' `update=` argument. They accomplish the same goal of updating column headers.
#' @param text_interpret String indicates whether text will be interpreted with
#' [gt::md()] or [gt::html()]. Must be `"md"` (default) or `"html"`.
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @author Daniel D. Sjoberg
#' @examples
#' # create summary table
#' tbl <- trial[c("age", "grade", "trt")] %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_p()
#'
#' # print `.$table_body` to show column names and update headers
#' tbl$table_body
#'
#' # Example 1 ----------------------------------
#' # updating column headers
#' modify_header_ex1 <-
#'   tbl %>%
#'   modify_header(
#'     update = list(
#'       label ~ "**Variable**",
#'       p.value ~ "**P**"
#'     )
#'   )
#'
#' # Example 2 ----------------------------------
#' # using `stat_by=` argument to update headers
#' modify_header_ex2 <-
#'   tbl %>%
#'   modify_header(
#'     stat_by = "**{level}**, N = {n} ({style_percent(p)}%)"
#'   )
#' @return Function return the same class of gtsummary object supplied
#' @export
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_header_ex1.png}{options: width=45\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{modify_header_ex2.png}{options: width=45\%}}

modify_header <- function(x, update = NULL, stat_by = NULL,
                          text_interpret = c("md", "html"), ...) {
  # converting update arg to a tidyselect list ---------------------------------
  update <-
    tidyselect_to_list(x$table_body, {{ update }}, arg_name = "update") %>%
    # adding the ... to the update list
    c(list(...))

  # running modify_header_internal function ------------------------------------
  rlang::call2(
    modify_header_internal,
    x = x,
    stat_by = stat_by,
    text_interpret = text_interpret,
    !!!update,
    .save_call = TRUE
  ) %>%
    eval()
}

