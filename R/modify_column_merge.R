#' Modify Column Merging
#'
#' \lifecycle{experimental}
#' Merge two or more columns in a gtsummary table.
#' Use `show_header_names()` to print underlying column names.
#' @param pattern glue syntax string indicating how to merge columns in
#' `x$table_body`. For example, to construct a confidence interval
#' use `"{conf.low}, {conf.high}"`.
#' @inheritParams modify_table_styling
#'
#' @section Details:
#' 1. Calling this function merely records the instructions to merge columns.
#' The actual merging occurs when the gtsummary table is printed or converted
#' with a function like `as_gt()`.
#' 2. Because the column merging is delayed, it is recommended to perform
#' major modifications to the table, such as those with `tbl_merge()` and
#' `tbl_stack()`, before assigning merging instructions. Otherwise,
#' unexpected formatting may occur in the final table.
#' 3. If this functionality is used in conjunction with `tbl_stack()` (which
#' includes `tbl_uvregression()`), there is potential issue with printing.
#' When columns are stack AND when the column-merging is
#' defined with a quosure, you may run into issues due to the loss of the
#' environment when 2 or more quosures are combined. If the expression
#' version of the quosure is the same as the quosure (i.e. no evaluated
#' objects), there should be no issues. Regardless, this argument is used
#' internally with care, and **it is _not_ recommended for users**.
#'
#' @section Future Updates:
#' There are planned updates to the implementation of this function
#' with respect to the `pattern=` argument.
#' Currently, this function replaces a numeric column with a
#' formatted character column following `pattern=`.
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
#'
#' @return gtsummary table
#' @export
#'
#' @family Advanced modifiers
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' modify_column_merge_ex1 <-
#'   trial %>%
#'   select(age, marker, trt) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_p(all_continuous() ~ "t.test",
#'     pvalue_fun = ~ style_pvalue(., prepend_p = TRUE)
#'   ) %>%
#'   modify_fmt_fun(statistic ~ style_sigfig) %>%
#'   modify_column_merge(pattern = "t = {statistic}; {p.value}") %>%
#'   modify_header(statistic ~ "**t-test**")
#'
#' # Example 2 ----------------------------------
#' modify_column_merge_ex2 <-
#'   lm(marker ~ age + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_column_merge(
#'     pattern = "{estimate} ({ci})",
#'     rows = !is.na(estimate)
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "modify_column_merge_ex1.png", width = "65")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "modify_column_merge_ex2.png", width = "41")`
#' }}
modify_column_merge <- function(x, pattern, rows = NULL) {
  # check inputs ---------------------------------------------------------------
  .assert_class(x, "gtsummary")
  if (!rlang::is_string(pattern)) abort("`pattern=` must be a string.")
  updated_call_list <- c(x$call_list, list(modify_column_hide = match.call()))

  # extract columns from pattern -----------------------------------------------
  columns <-
    pattern %>%
    str_extract_all("\\{.*?\\}") %>%
    map(~ str_remove_all(.x, pattern = "^\\{|\\}$")) %>%
    unlist()
  if (length(columns) == 0L) {
    cli::cli_alert_danger("No column names found in {.code modify_column_merge(pattern=)}")
    cli::cli_ul("Wrap all column names in curly brackets.")
    abort("Error in `pattern=` argument")
  }
  if (!all(columns %in% names(x$table_body))) {
    problem_cols <- columns %>% setdiff(names(x$table_body))
    paste(
      "Some columns specified in {.code modify_column_merge(pattern=)}",
      "were not found in the table, e.g. {.val {problem_cols}}"
    ) %>%
      cli::cli_alert_danger()
    cli::cli_ul("Select from {.val {names(x$table_body)}}.")
    abort("Error in `pattern=` argument")
  }

  # merge columns --------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = columns[1],
      rows = {{ rows }},
      hide = FALSE,
      cols_merge_pattern = pattern,
    )

  # return gtsummary table -----------------------------------------------------
  x$call_list <- updated_call_list
  x
}
