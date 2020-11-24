#' Tests available in `add_p()`
#'
#' @description Below is a listing of tests available internally within gtsummary.
#'
#' Tests listed with `...` may have additional arguments
#' passed to them using `add_p(test.args=)`. For example, to
#' calculate a p-value from `t.test()` assuming equal variance, use
#' `tbl_summary(trial, by = trt) %>% add_p(age ~ "t.test", test.args = age ~ list(var.equal = TRUE))`
#'
#' @name add_p_tests
#' @section tbl_summary() %>% add_p():
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_summary") %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {stringr::str_glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name, `**description**` = description, `**pseudo-code**` = pseudo_code) %>%
#'   knitr::kable()
#' ```
#'
#' @section tbl_svysummary() %>% add_p():
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_svysummary") %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {stringr::str_glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name, `**description**` = description, `**pseudo-code**` = pseudo_code) %>%
#'   knitr::kable()
#' ```
#'
#' @section tbl_survfit() %>% add_p():
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_survfit") %>%
#'   dplyr::mutate(test_name = shQuote(test_name) %>% {stringr::str_glue('`{.}`')}) %>%
#'   select(`**alias**` = test_name, `**description**` = description, `**pseudo-code**` = pseudo_code) %>%
#'   knitr::kable()
#' ```
#'
#' @section Custom Functions:
#'
#' To report a p-value for a test not available in gtsummary, you can create a
#' custom function. The output is a data frame that is one line long. The
#' structure is similar to the output of `broom::tidy()` of a typical
#' statistical test. The `add_p()` function will look for columns called
#' `"p.value"` and `"method"` for the p-value and the test name used in the footnote.
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
#' ### Function Arguments
#'
#' For `tbl_summary()` objects, the custom function will be passed the
#' following arguments: `custom_pvalue_fun(data=, variable=, by=, group=, type=)`.
#' While your function may not utilize each of these arguments, these arguments
#' are passed and the function must accept them. We recommend including `...`
#' to future-proof against updates where additional arguments are added.
#'
#' The following table describes the argument inputs for each gtsummary table type.
#'
#' ```{r, echo = FALSE}
#' tibble::tribble(
#'   ~`**argument**`, ~`**tbl_summary**`, ~`**tbl_svysummary**`, ~`**tbl_survfit**`,
#'   "`data=`", "A data frame", "A survey object", "A `survfit()` object",
#'   "`variable=`", "String variable name", "String variable name", "`NA`",
#'   "`by=`", "String variable name", "String variable name", "`NA`",
#'   "`group=`", "String variable name", "`NA`", "`NA`",
#'   "`type=`", "Summary type", "Summary type", "`NA`"
#' ) %>%
#' knitr::kable()
#' ```
NULL
