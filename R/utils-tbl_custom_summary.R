#' Summarize a continuous variable
#'
#' \lifecycle{experimental}
#' This helper, to be used with [tbl_custom_summary()], creates a function
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


#' Summarize the ratio of two variables
#'
#' \lifecycle{experimental}
#' This helper, to be used with [tbl_custom_summary()], creates a function
#' computing the ratio of two continuous variables and its confidence interval.
#'
#' @param numerator String indicating the name of the variable to be summed
#' for computing the numerator.
#' @param denominator String indicating the name of the variable to be summed
#' for computing the denominator.
#' @param na.rm Should missing values be removed before summing the numerator
#' and the denominator? (default is `TRUE`)
#' @param conf.level Confidence level for the returned confidence interval.
#' Must be strictly greater than 0 and les than 1. Default to 0.95, which
#' corresponds to a 95 percent confidence interval.
#'
#' @details
#' Computed statistics:
#' \itemize{
#'   \item `{num}` sum of the variable defined by `numerator`
#'   \item `{denom}` sum of the variable defined by `denominator`
#'   \item `{ratio}` ratio of `num` by `denom`
#'   \item `{conf.low}` lower confidence interval
#'   \item `{conf.high}` upper confidence interval
#' }
#'
#' Condidence interval is computed with [stats::poisson.test()], if and only if
#' `num` is an integer.
#'
#' @export
#' @family tbl_custom_summary tools
#' @author Joseph Larmarange
#' @examples
#' # Example 1 ----------------------------------
#' ratio_summary_ex1 <-
#'   trial %>%
#'   tbl_custom_summary(
#'     include = c("stage", "grade"),
#'     by = "trt",
#'     stat_fns = everything() ~ ratio_summary("response", "ttdeath"),
#'     statistic = everything() ~ "{ratio} [{conf.low}; {conf.high}] ({num}/{denom})",
#'     digits = everything() ~ c(3, 2, 2, 0, 0),
#'     overall_row = TRUE,
#'     overall_row_label = "All stages & grades"
#'   ) %>%
#'   bold_labels() %>%
#'   modify_footnote(
#'     update = all_stat_cols() ~ "Ratio [95% CI] (n/N)"
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{ratio_summary_ex1.png}{options: width=31\%}}
ratio_summary <- function(numerator, denominator, na.rm = TRUE, conf.level = 0.95) {
  function(data, ...) {
    num <- sum(data[[numerator]], na.rm = na.rm)
    denom <- sum(data[[denominator]], na.rm = na.rm)
    ratio <- num / denom
    if (num %% 1 == 0) {
      ci_poisson <- stats::poisson.test(num, denom, conf.level = conf.level)$conf.int
    } else {
      ci_poisson <- c(NA, NA)
    }
    dplyr::tibble(
      num = num,
      denom = denom,
      ratio = ratio,
      conf.low = ci_poisson[1],
      conf.high = ci_poisson[2]
    )
  }
}
