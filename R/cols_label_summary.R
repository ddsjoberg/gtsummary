#' Relabel columns in summary table
#'
#' Column labels can be modified to include calculated statistics,
#' e.g. the N and percent within each group
#'
#' @param x `tbl_summary` object
#' @param stat_by string vector of text to include above the summary statistics
#' stratified by a variable.  The following fields are available for use in the
#' headers: `{n}`, `{N}`, `{p}`, and `{level}`.  `{n}` is the number of observations in
#' each by group. `{N}` is the total number of observations. `{p}` is the percent
#' in a by group. `{level}` is the by variable level.
#' Syntax follows the [glue::glue] function,
#' e.g. `stat_by = md("**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")`.
#' The `by` argument from the parent `tbl_summary()` cannot be `NULL`.
#' @param stat_overall string vector including text to appear above the overall summary
#' statistics. `{N}`, the total number of observations, is available for use in the
#' description. e.g. `stat_overall = md("**All Patients**, N = {N}")`
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @seealso [gt::md], [gt::html]
#' @examples
#' tbl_col_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, response) %>%
#'   tbl_summary() %>%
#'   cols_label_summary(stat_overall = md("**All Patients**, N = {N}"))
#' tbl_col_ex2 <-
#'   trial %>%
#'   dplyr::select(age, grade, response, trt) %>%
#'   tbl_summary(by = "trt") %>%
#'   cols_label_summary(
#'     stat_by = md("**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")
#'    )
#' @export
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_col_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_col_ex2.png}{options: width=50\%}}

cols_label_summary <- function(x, stat_overall = NULL, stat_by = NULL) {
  # converting calls to un-evaluated string ------------------------------------
  stat_overall_str <- deparse(substitute(stat_overall))
  stat_by_str <- deparse(substitute(stat_by))

  # input checks ---------------------------------------------------------------
  # x input is of type tbl_summary
  if (class(x) != "tbl_summary") {
    stop("Class of 'x' must be tbl_summary")
  }
  # checking whether input is consistent with by variables
  if (!is.null(stat_by) & is.null(x$by)) {
    stop("Cannot specify 'stat_by' without first including 'by' in 'tbl_summary'")
  }
  if (!is.null(stat_overall) & !is.null(x$by) & is.null(x$call_list$add_overall)) {
    stop(glue(
      "Cannot specify 'stat_overall' when no overall statistics are present in 'tbl_summary'"
    ))
  }

  # stat_by --------------------------------------------------------------------
  if (!is.null(stat_by)) {
    x[["gt_calls"]][["cols_label_by"]] <- col_label_by(x$df_by, stat_by_str)
  }

  # stat_overall ---------------------------------------------------------------
  if (!is.null(stat_overall)) {
    x[["gt_calls"]][["cols_label_overall"]] <-
      col_label_overall(stat_overall_str, x$inputs$data %>% nrow())
  }

  x
}

col_label_by <- function(df_by, stat_by) {
  df_by %>%
    select(c("by_col", "by_chr", "n", "N", "p")) %>%
    set_names(c("by_col", "level", "n", "N", "p")) %>%
    mutate(
      col_label = glue(stat_by),
      col_label_code = glue("{by_col} = {col_label}")
    ) %>%
    pull("col_label_code") %>%
    paste(collapse = ", ") %>%
    {
      glue("cols_label({.})")
    }
}

col_label_overall <- function(stat_overall, N) {
  glue(stat_overall) %>%
  {glue("cols_label(stat_0 = {.})")}
}

