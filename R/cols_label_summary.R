#' Relabel columns in summary table
#'
#' Column labels can be modified using data from the data frame
#'
#' @param x `tbl_summary` object
#' @param stat_by string vector of text to include above the summary statistics
#' stratified by a variable.  The following fields are available for use in the
#' headers: `n`, `N`, `p`, and `level`.  `n` is the number of observations in
#' each by group. `N` is the total number of observations. `p` is the percent
#' in a by group. `level` is the by variable level.
#' Syntax follows the \code{glue::\link{glue}} function,
#' e.g. `stat_by = md("**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")`.
#' Must specify `by` along with `stat_by`.
#' @param stat_overall string vector including text to appear above the overall summary
#' statistics. `N`, the total number of observations, is available for use in the
#' description. e.g. `stat_overall = md("**All Patients**, N = {N}")`
#' @author Daniel Sjoberg
#' @seealso \link{tbl_summary}, \link[gt]{md}, \link[gt]{html}
#' @examples
#' trial %>%
#'   tbl_summary() %>%
#'   cols_label_summary(stat_overall = md("**All Patients**, N = {N}"))
#' trial %>%
#'   tbl_summary(by = "trt") %>%
#'   cols_label_summary(stat_by = md("**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})"))
#' @export


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
    dplyr::select(c("by_col", "by_chr", "n", "N", "p")) %>%
    rlang::set_names(c("by_col", "level", "n", "N", "p")) %>%
    dplyr::mutate_(
      col_label = ~ glue::glue(stat_by),
      col_label_code = ~ glue::glue("{by_col} = {col_label}")
    ) %>%
    dplyr::pull("col_label_code") %>%
    paste(collapse = ", ") %>%
    {
      glue::glue("cols_label({.})")
    }
}

col_label_overall <- function(stat_overall, N) {
  glue::glue(stat_overall) %>%
  {glue::glue("cols_label(stat_0 = {.})")}
}

