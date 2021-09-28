#' Summarize a continuous variable
#'
#' \lifecycle{experimental}
#' This helper to be used with [tbl_custom_summary()] creates a function
#' summarizing a continuous variable.
#'
#' @param variable String indicating the name of the variable to be summarized. This
#' variable should be continuous.
#'
#' @details
#' When using `continuous_summary`, you can specify in the `statistic=` argument
#' of [tbl_custom_summary()] the same continuous statistics than in
#' [tbl_summary()]. See the *statistic argument* section of the help file of
#' [tbl_summary()].
#'
#' @export
#' @family tbl_custom_summary tools
#' @author Joseph Larmarange
#' @examples
#' # Example 1 ----------------------------------
#' continuous_summary_ex1 <-
#'   trial %>%
#'   tbl_custom_summary(
#'     include = c("stage", "grade"),
#'     by = "trt",
#'     stat_fns = everything() ~ continuous_summary("age"),
#'     statistic = everything() ~ "{median} [{p25}-{p75}]",
#'     overall_row = TRUE,
#'     overall_row_label = "All stages & grades"
#'   ) %>%
#'   modify_footnote(
#'     update = all_stat_cols() ~ "Median age (IQR)"
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{continuous_summary_ex1.png}{options: width=31\%}}
continuous_summary <- function(variable) {
  variable_to_summarize <- variable
  function(data, stat_display, ...) {
    summarize_continuous(
      data = data,
      variable = variable_to_summarize,
      by = NULL,
      stat_display = stat_display,
      summary_type = "continuous"
    ) %>%
      dplyr::select(-.data$variable, -.data$stat_display)
  }
}
