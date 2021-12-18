#' Common Sources of Error with `tbl_survfit()`
#'
#' @description When functions `add_n()` and `add_p()` are run after `tbl_survfit()`,
#' the original call to `survival::survfit()`
#' is extracted and the `formula=` and `data=` arguments are used to calculate
#' the N or p-value.
#'
#' When the values of the `formula=` and `data=` are unavailable, the functions
#' cannot execute. Below are some tips to modify your code to ensure all functions
#' run without issue.
#'
#' 1. Let `tbl_survfit()` construct the `survival::survfit()` for you by passing
#' a data frame to `tbl_survfit()`. The survfit model will be constructed
#' in a manner ensuring the formula and data are available.
#' This only works if you have a stratified model.
#'
#'     Instead of the following line
#'
#'     ```r
#'     survfit(Surv(ttdeath, death) ~ trt, trial) %>%
#'       tbl_survfit(times = c(12, 24))
#'     ```
#'     Use this code
#'
#'     ```r
#'     trial %>%
#'       select(ttdeath, death, trt) %>%
#'       tbl_survfit(y = Surv(ttdeath, death), times = c(12, 24))
#'     ```
#'
#' 2. Construct an expression of the `survival::survfit()` before evaluating it.
#' Ensure the formula and data are available in the call by using the tidyverse
#' bang-bang operator, `!!`.
#'
#'     Use this code
#'
#'     ```r
#'     formula_arg <- Surv(ttdeath, death) ~ 1
#'     data_arg <- trial
#'     rlang::expr(survfit(!!formula_arg, !!data_arg)) %>%
#'       eval() %>%
#'       tbl_survfit(times = c(12, 24))
#'     ```
#' @keywords internal
#' @name tbl_survfit_errors
NULL
