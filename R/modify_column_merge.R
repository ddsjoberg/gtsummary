#' Modify Column Merging
#'
#' Merge two or more columns in a gtsummary table.
#' Use `show_header_names()` to print underlying column names.
#'
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
#' includes `tbl_uvregression()`), there may be potential issues with printing.
#' When columns are stack AND when the column-merging is
#' defined with a quosure, you may run into issues due to the loss of the
#' environment when 2 or more quosures are combined. If the expression
#' version of the quosure is the same as the quosure (i.e. no evaluated
#' objects), there should be no issues.
#'
#' This function is used internally with care, and **it is _not_ recommended for users**.
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
#' @return gtsummary table
#' @export
#'
#' @family Advanced modifiers
#' @examplesIf gtsummary:::is_pkg_installed("cardx") && gtsummary:::is_pkg_installed("broom", ref = "cardx")
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(by = trt, missing = "no", include = c(age, marker, trt)) |>
#'   add_p(all_continuous() ~ "t.test", pvalue_fun = label_style_pvalue(prepend_p = TRUE)) |>
#'   modify_fmt_fun(statistic ~ label_style_sigfig()) |>
#'   modify_column_merge(pattern = "t = {statistic}; {p.value}") |>
#'   modify_header(statistic = "**t-test**")
#'
#' # Example 2 ----------------------------------
#' lm(marker ~ age + grade, trial) |>
#'   tbl_regression() |>
#'   modify_column_merge(
#'     pattern = "{estimate} ({conf.low}, {conf.high})",
#'     rows = !is.na(estimate)
#'   )
modify_column_merge <- function(x, pattern, rows = NULL) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_string(pattern)
  updated_call_list <- c(x$call_list, list(modify_column_hide = match.call()))

  # extract columns from pattern -----------------------------------------------
  columns <- .extract_glue_elements(pattern)
  if (is_empty(columns)) {
    cli::cli_abort(
      c("No column names found in {.code modify_column_merge(pattern)} argument.",
        i = "Wrap column names in curly brackets, e.g {.code modify_column_merge(pattern = '{{conf.low}}, {{conf.high}}')}."
      ),
      call = get_cli_abort_call()
    )
  }
  if (!all(columns %in% names(x$table_body))) {
    problem_cols <- columns %>% setdiff(names(x$table_body))
    cli::cli_abort(
      c(
        "Columns specified in the {.code modify_column_merge(pattern)} argument are not present in table.",
        "Columns {.val {problem_cols}} not found."
      ),
      call = get_cli_abort_call()
    )
  }

  # merge columns --------------------------------------------------------------
  x <- x |>
    # remove prior merging for the specified columns
    modify_table_styling(
      columns = all_of(columns),
      rows = {{ rows }},
      cols_merge_pattern = NA,
    ) |>
    # add the newly specified pattern of merging
    modify_table_styling(
      columns = columns[1],
      rows = {{ rows }},
      hide = FALSE,
      cols_merge_pattern = pattern,
    )

  # return gtsummary table -----------------------------------------------------
  x$call_list <- updated_call_list
  x
}
