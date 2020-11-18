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
#'   select(alias = test_name, description, `pseudo-code` = pseudo_code) %>%
#'   dplyr::mutate(alias = shQuote(alias) %>% {stringr::str_glue('`{.}`')}) %>%
#'   knitr::kable()
#' ```
#'
#' @section tbl_svysummary() %>% add_p():
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_svysummary") %>%
#'   select(alias = test_name, description, `pseudo-code` = pseudo_code) %>%
#'   dplyr::mutate(alias = shQuote(alias) %>% {stringr::str_glue('`{.}`')}) %>%
#'   knitr::kable()
#' ```
#'
#' @section tbl_survfit() %>% add_p():
#'
#' ```{r, echo = FALSE}
#' gtsummary:::df_add_p_tests %>%
#'   dplyr::filter(class == "tbl_survfit") %>%
#'   select(alias = test_name, description, `pseudo-code` = pseudo_code) %>%
#'   dplyr::mutate(alias = shQuote(alias) %>% {stringr::str_glue('`{.}`')}) %>%
#'   knitr::kable()
#' ```
NULL
