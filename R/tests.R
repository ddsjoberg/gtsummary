#' Tests/methods available in `add_p()` and `add_difference()`
#'
#' @description Below is a listing of tests available internally within gtsummary.
#'
#' Tests listed with `...` may have additional arguments
#' passed to them using `add_p(test.args=)`. For example, to
#' calculate a p-value from `t.test()` assuming equal variance, use
#' `tbl_summary(trial, by = trt) %>% add_p(age ~ "t.test", test.args = age ~ list(var.equal = TRUE))`
#'
#' @name tests
#' @keywords internal
#' @section `tbl_summary() %>% add_p()`:
#'
#' ```{r, echo = FALSE}
#' options(knitr.kable.NA = '')
#' remove_na_details_column <- function(data) {
#'   if (all(is.na(data[["**details**"]]))) return(dplyr::select(data, -`**details**`))
#'   data
#' }
#'
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_summary", add_p == TRUE, !test_name %in% 'aov') %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {glue::glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name,
#'          `**description**` = description,
#'          `**pseudo-code**` = pseudo_code,
#'          `**details**` = details) %>%
#'   remove_na_details_column() %>%
#'   knitr::kable()
#' ```
#'
#' @section `tbl_svysummary() %>% add_p()`:
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_svysummary", add_p == TRUE, !test_name %in% 'aov') %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {glue::glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name,
#'          `**description**` = description,
#'          `**pseudo-code**` = pseudo_code,
#'          `**details**` = details) %>%
#'   remove_na_details_column() %>%
#'   knitr::kable()
#' ```
#'
#' @section `tbl_survfit() %>% add_p()`:
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_survfit", add_p == TRUE, !test_name %in% 'aov') %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {glue::glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name,
#'          `**description**` = description,
#'          `**pseudo-code**` = pseudo_code,
#'          `**details**` = details) %>%
#'   remove_na_details_column() %>%
#'   knitr::kable()
#' ```
#'
#' @section `tbl_continuous() %>% add_p()`:
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_continuous", add_p == TRUE, !test_name %in% 'aov') %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {glue::glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name,
#'          `**description**` = description,
#'          `**pseudo-code**` = pseudo_code,
#'          `**details**` = details) %>%
#'   remove_na_details_column() %>%
#'   knitr::kable()
#' ```
#'
#' @section tbl_summary() %>% add_difference():
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_summary", add_difference == TRUE, !test_name %in% 'aov') %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {glue::glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name,
#'          `**description**` = description,
#'          `**difference statistic**` = diff_statistic,
#'          `**pseudo-code**` = pseudo_code,
#'          `**details**` = details) %>%
#'   remove_na_details_column() %>%
#'   knitr::kable()
#' ```
#'
#' @section tbl_svysummary() %>% add_difference():
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_svysummary", add_difference == TRUE, !test_name %in% 'aov') %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {glue::glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name,
#'          `**description**` = description,
#'          `**difference statistic**` = diff_statistic,
#'          `**pseudo-code**` = pseudo_code,
#'          `**details**` = details) %>%
#'   remove_na_details_column() %>%
#'   knitr::kable()
#' ```
#'
#' @section Custom Functions:
#'
#' To report a p-value (or difference) for a test not available in gtsummary, you can create a
#' custom function. The output is a data frame that is one line long. The
#' structure is similar to the output of `broom::tidy()` of a typical
#' statistical test. The `add_p()` and `add_difference()` functions will look for columns called
#' `"p.value"`, `"estimate"`, `"statistic"`, `"std.error"`, `"parameter"`,
#' `"conf.low"`, `"conf.high"`, and `"method"`.
#'
#' You can also pass an Analysis Results Dataset (ARD) object with the results
#' for your custom result. These objects follow the structures outlined
#' by the \{cards\} and \{cardx\} packages.
#'
#' Example calculating a p-value from a t-test assuming a common variance
#' between groups.
#'
#' ```r
#' ttest_common_variance <- function(data, variable, by, ...) {
#'   data <- data[c(variable, by)] %>% dplyr::filter(complete.cases(.))
#'   t.test(data[[variable]] ~ factor(data[[by]]), var.equal = TRUE) %>%
#'   broom::tidy()
#' }
#'
#' trial[c("age", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p(test = age ~ "ttest_common_variance")
#' ```
#'
#' A custom `add_difference()` is similar, and accepts arguments `conf.level=`
#' and `adj.vars=` as well.
#'
#' ```r
#' ttest_common_variance <- function(data, variable, by, conf.level, ...) {
#'   data <- data[c(variable, by)] %>% dplyr::filter(complete.cases(.))
#'   t.test(data[[variable]] ~ factor(data[[by]]), conf.level = conf.level, var.equal = TRUE) %>%
#'   broom::tidy()
#' }
#' ```
#'
#' ### Function Arguments
#'
#' For `tbl_summary()` objects, the custom function will be passed the
#' following arguments: `custom_pvalue_fun(data=, variable=, by=, group=, type=, conf.level=, adj.vars=)`.
#' While your function may not utilize each of these arguments, these arguments
#' are passed and the function must accept them. We recommend including `...`
#' to future-proof against updates where additional arguments are added.
#'
#' The following table describes the argument inputs for each gtsummary table type.
#'
#' ```{r, echo = FALSE}
#' dplyr::tribble(
#'   ~`**argument**`, ~`**tbl_summary**`, ~`**tbl_svysummary**`, ~`**tbl_survfit**`, ~`**tbl_continuous**`,
#'   "`data=`", "A data frame", "A survey object", "A `survfit()` object", "A data frame",
#'   "`variable=`", "String variable name", "String variable name", "`NA`", "String variable name",
#'   "`by=`", "String variable name", "String variable name", "`NA`", "String variable name",
#'   "`group=`", "String variable name", "`NA`", "`NA`", "String variable name",
#'   "`type=`", "Summary type", "Summary type", "`NA`", "`NA`",
#'   "`conf.level=`", "Confidence interval level", "`NA`", "`NA`", "`NA`",
#'   "`adj.vars=`", "Character vector of adjustment variable names (e.g. used in ANCOVA)", "`NA`", "`NA`", "Character vector of adjustment variable names (e.g. used in ANCOVA)",
#'   "`continuous_variable=`", "`NA`", "`NA`", "`NA`", "String of the continuous variable name"
#' ) %>%
#' knitr::kable()
#' ```
NULL
