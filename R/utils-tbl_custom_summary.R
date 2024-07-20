#' Summarize a continuous variable
#'
#' \lifecycle{deprecated}
#' This helper, to be used with [`tbl_custom_summary()`], creates a function
#' summarizing a continuous variable.
#'
#' @param variable (`string`)\cr
#'   String indicating the name of the variable to be summarized. This
#'   variable should be continuous.
#'
#' @details
#' When using `continuous_summary()`, you can specify in the `statistic=` argument
#' of [`tbl_custom_summary()`] the same continuous statistics than in
#' [`tbl_summary()`]. See the *statistic argument* section of the help file of
#' [`tbl_summary()`].
#'
#' @export
#' @return NULL
#' @keywords internal
#'
#' @author Joseph Larmarange
continuous_summary <- function(variable) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "gtsummary::continuous_summary()",
    details = I("Please use `tbl_continuous()` instead, which can be a drop-in replacement in most cases.")
  )
}


#' Summarize the ratio of two variables
#'
#' \lifecycle{experimental}
#' This helper, to be used with [tbl_custom_summary()], creates a function
#' computing the ratio of two continuous variables and its confidence interval.
#'
#' @param numerator (`string`)\cr
#'   String indicating the name of the variable to be summed for computing the numerator.
#' @param denominator (`string`)\cr
#'   String indicating the name of the variable to be summed
#'   for computing the denominator.
#' @param na.rm (scalar `logical`)\cr
#'   Should missing values be removed before summing the numerator
#'   and the denominator? (default is `TRUE`)
#' @param conf.level (scalar `numeric`)\cr
#'   Confidence level for the returned confidence interval.
#'   Must be strictly greater than 0 and less than 1. Default to 0.95, which
#'   corresponds to a 95 percent confidence interval.
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
#'
#' @author Joseph Larmarange
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_custom_summary(
#'     include = c("stage", "grade"),
#'     by = "trt",
#'     stat_fns = ~ ratio_summary("response", "ttdeath"),
#'     statistic = ~"{ratio} [{conf.low}; {conf.high}] ({num}/{denom})",
#'     digits = ~ c(ratio = 3, conf.low = 2, conf.high = 2),
#'     overall_row = TRUE,
#'     overall_row_label = "All stages & grades"
#'   ) |>
#'   bold_labels() |>
#'   modify_footnote(all_stat_cols() ~ "Ratio [95% CI] (n/N)")
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
#' @param variable (`string`)\cr
#'   String indicating the name of the variable from which the proportion will be computed.
#' @param value (`scalar`)\cr
#'   Value (or list of values) of `variable` to be taken into account in the numerator.
#' @param weights (`string`)\cr
#'   Optional string indicating the name of a frequency weighting variable.
#'   If `NULL`, all observations will be assumed to have a weight equal to `1`.
#' @param na.rm (scalar `logical`)\cr
#'   Should missing values be removed before computing the proportion? (default is `TRUE`)
#' @param conf.level (scalar `numeric`)\cr
#'   Confidence level for the returned confidence interval.
#'   Must be strictly greater than 0 and less than 1. Default to 0.95, which
#'   corresponds to a 95 percent confidence interval.
#' @param method (`string`)\cr
#'   Confidence interval method. Must be one of
#'   `c("wilson", "wilson.no.correct", "wald", "wald.no.correct", "exact", "agresti.coull", "jeffreys")`.
#'   See `add_ci()` for details.
#'
#' @details
#' Computed statistics:
#' \itemize{
#'   \item `{n}` numerator, number of observations equal to `values`
#'   \item `{N}` denominator, number of observations
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
#' @author Joseph Larmarange
#' @examples
#' # Example 1 ----------------------------------
#' Titanic |>
#'   as.data.frame() |>
#'   tbl_custom_summary(
#'     include = c("Age", "Class"),
#'     by = "Sex",
#'     stat_fns = ~ proportion_summary("Survived", "Yes", weights = "Freq"),
#'     statistic = ~ "{prop}% ({n}/{N}) [{conf.low}-{conf.high}]",
#'     digits = ~ list(
#'       prop = label_style_percent(digits = 1),
#'       n = 0,
#'       N = 0,
#'       conf.low = label_style_percent(),
#'       conf.high = label_style_percent()
#'     ),
#'     overall_row = TRUE,
#'     overall_row_last = TRUE
#'   ) |>
#'   bold_labels() |>
#'   modify_footnote(all_stat_cols() ~ "Proportion (%) of survivors (n/N) [95% CI]")
proportion_summary <- function(variable, value, weights = NULL, na.rm = TRUE, conf.level = 0.95,
                               method = c("wilson", "wilson.no.correct", "wald", "wald.no.correct", "exact", "agresti.coull", "jeffreys")) {
  # process arguments ----------------------------------------------------------
  if (identical(method, "asymptotic")) method <- "wald.no.correct" # Documentation of "asymptotic" was removed in v2.0.0
  else method <- arg_match(method)

  check_scalar_range(conf.level, range = c(0, 1))
  check_scalar_logical(na.rm)
  check_not_missing(variable)
  check_string(variable)
  check_string(weights, allow_empty = TRUE)


  function(data, ...) {
    # create lgl vector of variable to calculate CI
    if (!is_empty(weights)) {
      data <- data[c(variable, weights)] |> tidyr::uncount(weights = !!sym(weights))
    }
    if (na.rm) data <- tidyr::drop_na(data, all_of(variable))
    x <- data[[variable]] %in% value

    switch(
      method,
      "wilson" = cardx::proportion_ci_wilson(x = x, conf.level = conf.level, correct = TRUE),
      "wilson.no.correct" = cardx::proportion_ci_wilson(x = x, conf.level = conf.level, correct = FALSE),
      "wald" = cardx::proportion_ci_wald(x = x, conf.level = conf.level, correct = TRUE),
      "wald.no.correct" = cardx::proportion_ci_wald(x = x, conf.level = conf.level, correct = FALSE),
      "exact" = cardx::proportion_ci_clopper_pearson(x = x, conf.level = conf.level),
      "agresti.coull" = cardx::proportion_ci_agresti_coull(x = x, conf.level = conf.level),
      "jeffreys" = cardx::proportion_ci_jeffreys(x = x, conf.level = conf.level),
      # Documentation of 'asymptotic' was removed in v2.0
      "asymptotic" = cardx::proportion_ci_wilson(x = x, conf.level = conf.level, correct = FALSE)
    ) %>%
      {dplyr::tibble(!!!.)} |>
      dplyr::select(any_of(c("N", "estimate", "conf.low", "conf.high"))) |>
      dplyr::rename(prop = "estimate") |>
      dplyr::mutate(n = .data$N * .data$prop, .before = 0L)
  }
}
