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
#' \donttest{
#' # Example 1 ----------------------------------
#' continuous_summary_ex1 <-
#'   trial %>%
#'   tbl_custom_summary(
#'     include = c("stage", "grade"),
#'     by = "trt",
#'     stat_fns = ~ continuous_summary("age"),
#'     statistic = ~"{median} [{p25}-{p75}]",
#'     overall_row = TRUE,
#'     overall_row_label = "All stages & grades"
#'   ) %>%
#'   modify_footnote(
#'     update = all_stat_cols() ~ "Median age (IQR)"
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "continuous_summary_ex1.png", width = "31")`
#' }}
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
      dplyr::select(-"variable", -"stat_display")
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
#' Must be strictly greater than 0 and less than 1. Default to 0.95, which
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
#' Confidence interval is computed with [stats::poisson.test()], if and only if
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
#'     stat_fns = ~ ratio_summary("response", "ttdeath"),
#'     statistic = ~"{ratio} [{conf.low}; {conf.high}] ({num}/{denom})",
#'     digits = ~ c(3, 2, 2, 0, 0),
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
#' \if{html}{\out{
#' `r man_create_image_tag(file = "ratio_summary_ex1.png", width = "31")`
#' }}
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




#' Summarize a proportion
#'
#' \lifecycle{experimental}
#' This helper, to be used with [tbl_custom_summary()], creates a function
#' computing a proportion and its confidence interval.
#'
#' @param variable String indicating the name of the variable from which the
#' proportion will be computed.
#' @param value Value (or list of values) of `variable` to be taken into account
#' in the numerator.
#' @param weights Optional string indicating the name of a weighting variable.
#' If `NULL`, all observations will be assumed to have a weight equal to `1`.
#' @param na.rm Should missing values be removed before computing the
#' proportion? (default is `TRUE`)
#' @param conf.level Confidence level for the returned confidence interval.
#' Must be strictly greater than 0 and less than 1. Default to 0.95, which
#' corresponds to a 95 percent confidence interval.
#' @param method Confidence interval method. Must be one of
#' `c("wilson", "wilson.no.correct", "exact", "asymptotic")`. See details below.
#'
#' @details
#' Computed statistics:
#' \itemize{
#'   \item `{n}` numerator, (weighted) number of observations equal to `values`
#'   \item `{N}` denominator, (weighted) number of observations
#'   \item `{prop}` proportion, i.e. `n/N`
#'   \item `{conf.low}` lower confidence interval
#'   \item `{conf.high}` upper confidence interval
#' }
#'
#' Methods `c("wilson", "wilson.no.correct")` are calculated with
#' [stats::prop.test()] (with `correct = c(TRUE, FALSE)`). The default method,
#' `"wilson"`, includes the Yates continuity correction.
#' Methods `c("exact", "asymptotic")` are calculated with `Hmisc::binconf()`
#' and the corresponding method.
#'
#' @export
#' @family tbl_custom_summary tools
#' @author Joseph Larmarange
#' @examples
#' # Example 1 ----------------------------------
#' proportion_summary_ex1 <-
#'   Titanic %>%
#'   as.data.frame() %>%
#'   tbl_custom_summary(
#'     include = c("Age", "Class"),
#'     by = "Sex",
#'     stat_fns = ~ proportion_summary("Survived", "Yes", weights = "Freq"),
#'     statistic = ~"{prop}% ({n}/{N}) [{conf.low}-{conf.high}]",
#'     digits = ~ list(
#'       function(x) {
#'         style_percent(x, digits = 1)
#'       },
#'       0, 0, style_percent, style_percent
#'     ),
#'     overall_row = TRUE,
#'     overall_row_last = TRUE
#'   ) %>%
#'   bold_labels() %>%
#'   modify_footnote(
#'     update = all_stat_cols() ~ "Proportion (%) of survivors (n/N) [95% CI]"
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "proportion_summary_ex1.png", width = "31")`
#' }}
proportion_summary <- function(variable, value, weights = NULL, na.rm = TRUE,
                               conf.level = 0.95,
                               method = c("wilson", "wilson.no.correct", "exact", "asymptotic")) {
  method <- match.arg(method)
  variable_to_summarize <- variable
  function(data, ...) {
    if (is.null(weights)) {
      n <- sum(data[[variable_to_summarize]] %in% value, na.rm = na.rm)
      N <- sum(!is.na(data[[variable_to_summarize]]), na.rm = na.rm)
    } else {
      n <- sum((data[[variable_to_summarize]] %in% value) * data[[weights]], na.rm = na.rm)
      N <- sum((!is.na(data[[variable_to_summarize]])) * data[[weights]], na.rm = na.rm)
    }
    if (anyNA(n, N)) {
      ci <- c(NA, NA)
    } else {
      if (method %in% c("wilson", "wilson.no.correct")) {
        ci <-
          stats::prop.test(n, N,
            conf.level = conf.level,
            correct = isTRUE(method == "wilson")
          ) %>%
          purrr::pluck("conf.int")
      } else if (method %in% c("exact", "asymptotic")) {
        assert_package("Hmisc", fn = 'proportion_summary(method = c("exact", "asymptotic"))')
        ci <-
          Hmisc::binconf(n, N,
            method = method, alpha = 1 - conf.level
          )[2:3]
      }
    }
    dplyr::tibble(
      n = n,
      N = N,
      prop = n / N,
      conf.low = ci[1],
      conf.high = ci[2]
    )
  }
}
