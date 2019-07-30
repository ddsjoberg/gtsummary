#' Relabel columns in summary table
#'
#' Column labels can be modified to include calculated statistics,
#' e.g. the N and percent within each group.  For {gt} output, all column labels
#' are wrapped in `gt::md()` allowing for the use of standard markdown syntax.
#'
#' @param x `tbl_summary` object
#' @param stat_by string vector of text to include above the summary statistics
#' stratified by a variable.  The following fields are available for use in the
#' headers: `{n}`, `{N}`, `{p}`, and `{level}`.  `{n}` is the number of observations in
#' each by group. `{N}` is the total number of observations. `{p}` is the percent
#' in a by group. `{level}` is the by variable level.
#' Syntax follows the [glue::glue] function,
#' e.g. `stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})"`.
#' The `by` argument from the parent `tbl_summary()` cannot be `NULL`.
#' @param stat_overall string vector including text to appear above the overall summary
#' statistics. `{N}`, the total number of observations, is available for use in the
#' description. e.g. `stat_overall = "**All Patients**, N = {N}"`
#' @param ... specify column label of any other column in `.$table_body`.`
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @examples
#' tbl_col_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, response) %>%
#'   tbl_summary() %>%
#'   cols_label_summary(stat_overall = "**All Patients**, N = {N}")
#' tbl_col_ex2 <-
#'   trial %>%
#'   dplyr::select(age, grade, response, trt) %>%
#'   tbl_summary(by = "trt") %>%
#'   cols_label_summary(
#'     stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})"
#'   )
#' @export
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_col_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_col_ex2.png}{options: width=50\%}}

cols_label_summary <- function(x, stat_overall = NULL, stat_by = NULL, ...) {
  # input checks ---------------------------------------------------------------
  # x input is of type tbl_summary
  if (class(x) != "tbl_summary") {
    stop("Class of 'x' must be tbl_summary")
  }
  # checking whether input is consistent with by variables
  if (!is.null(stat_by) & is.null(x$by)) {
    stop("Cannot specify 'stat_by' without first including 'by' in 'tbl_summary'")
  }
  if (!is.null(stat_overall) && !"stat_0" %in% names(x$table_body)) {
    stop(glue(
      "Cannot specify 'stat_overall' when no overall statistics are present in 'tbl_summary'"
    ))
  }

  # stat_by --------------------------------------------------------------------
  if (!is.null(stat_by)) {
    if (!rlang::is_string(stat_by)) {
      "'stat_by' must be a string of length one."
    }
    x$table_header <-
      x$df_by %>%
      rename(level = .data$by_chr) %>%
      mutate(label = glue(stat_by)  %>% as.character()) %>%
      select(column = .data$by_col, .data$label) %>%
      bind_rows(x$table_header)
  }

  # stat_overall ---------------------------------------------------------------
  if (!is.null(stat_overall)) {
    if (!rlang::is_string(stat_overall)) {
      "'stat_overall' must be a string of length one."
    }
    x$table_header <-
      tibble(column = "stat_0", N = nrow(x$inputs$data)) %>%
      mutate(label = glue(stat_overall) %>% as.character()) %>%
      select(.data$column, .data$label) %>%
      bind_rows(x$table_header)
  }

  # mapping over dots and updating labels --------------------------------------
  if (!rlang::is_empty(list(...))) {
    # saving dots arguments as a named list
    dots <-
      substitute(list(...))[-1] %>%
      sapply(I)

    if (names(dots) %>% setdiff(names(x$table_body)) %>% length() > 0) {
      stop(glue(
        "{names(dots) %>% setdiff(names(x$table_body)) %>% glue_collapse(sep = ', ')}, ",
        "are not column names in 'table_body'"
      ))
    }
    if (map_lgl(dots, ~!rlang::is_string(.)) %>% any()) {
      stop("All arguments passed via '...' must be strings of length one.")
    }

    x$table_header <-
      tibble(
        column = names(dots),
        label = dots
      ) %>%
      bind_rows(x$table_header)
  }

  # keeping updated labels -----------------------------------------------------
  x$table_header <-
    x$table_header %>%
    group_by(.data$column) %>%
    slice(1) %>%
    ungroup()

  # updating function calls ----------------------------------------------------
  x[["gt_calls"]][["cols_label"]] <-
   table_header_to_gt(x$table_header)

  # keeping track of all functions previously run ------------------------------
  x$call_list <- c(x$call_list, list(cols_label_summary = match.call()))

  x
}
