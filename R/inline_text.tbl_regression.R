#' Report statistics from regression summary tables inline
#'
#' Takes an object with class `tbl_regression`, and the
#' location of the statistic to report and returns statistics for reporting
#' inline in an R markdown document.  Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @inheritParams inline_text.gtsummary
#' @param x (`tbl_regression`)\cr
#'   Object created by [`tbl_regression()`]
#' @param pattern (`string`)\cr
#'   String indicating the statistics to return.
#'   Uses [`glue::glue()`] formatting.
#'   Default is `"{estimate} ({conf.level }\% CI  {conf.low}, {conf.high}; {p.value})"`.
#'   All columns from `x$table_body` are available to print as well as the
#'   confidence level (`conf.level`). See below for details.
#' @param estimate_fun (`function`)\cr
#'   Function to style model coefficient estimates.
#'   Columns `'estimate'`, `'conf.low'`, and `'conf.high'` are formatted.
#'   Default is `x$inputs$estimate_fun`
#' @param pvalue_fun function to style p-values and/or q-values.
#' Default is `label_style_pvalue(prepend_p = TRUE)`
#' @inheritParams rlang::args_dots_empty
#'
#' @section pattern argument:
#' The following items (and more) are available to print.  Use `print(x$table_body)` to
#' print the table the estimates are extracted from.
#' \itemize{
#'   \item `{estimate}` coefficient estimate formatted with 'estimate_fun'
#'   \item `{conf.low}` lower limit of confidence interval formatted with 'estimate_fun'
#'   \item `{conf.high}` upper limit of confidence interval formatted with 'estimate_fun'
#'   \item `{p.value}` p-value formatted with 'pvalue_fun'
#'   \item `{N}` number of observations in model
#'   \item `{label}` variable/variable level label
#' }
#' @author Daniel D. Sjoberg
#'
#' @export
#' @return A string reporting results from a gtsummary table
#' @examplesIf gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"), reference_pkg = "gtsummary")
#' inline_text_ex1 <-
#'   glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' inline_text(inline_text_ex1, variable = age)
#' inline_text(inline_text_ex1, variable = grade, level = "III")
inline_text.tbl_regression <- function(x,
                                       variable,
                                       level = NULL,
                                       pattern = "{estimate} ({conf.level*100}% CI {conf.low}, {conf.high}; {p.value})",
                                       estimate_fun = x$inputs$estimate_fun,
                                       pvalue_fun = label_style_pvalue(prepend_p = TRUE),
                                       ...) {
  set_cli_abort_call()
  check_dots_empty()

  # setting defaults -----------------------------------------------------------
  pvalue_fun <-
    case_switch(
      missing(pvalue_fun) ~ get_theme_element("pkgwide-fn:prependpvalue_fun", default = pvalue_fun),
      .default = pvalue_fun
    )
  pvalue_fun <- as_function(pvalue_fun)

  x <- modify_fmt_fun(x, any_of(c("p.value", "q.value")) ~ pvalue_fun)

  estimate_fun <- as_function(estimate_fun)
  x <- modify_fmt_fun(x, any_of(c("estimate", "conf.low", "conf.high")) ~ estimate_fun)

  x <- x |>
    modify_table_body(~ .x |> dplyr::mutate(conf.level = x$inputs$conf.level)) |>
    modify_fmt_fun(conf.level = as.numeric)

  # call generic inline_text() function ----------------------------------------
  inline_text.gtsummary(
    x = x,
    variable = {{ variable }},
    level = {{ level }},
    pattern = pattern
  )
}
