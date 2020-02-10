## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----exit_early, include = FALSE, eval = !requireNamespace("gt")--------------
#  knitr::knit_exit()

## ---- include=FALSE-----------------------------------------------------------
library(gtsummary)
library(dplyr)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("gtsummary")
#  remotes::install_github("rstudio/gt")
#  
#  library(gtsummary)
#  library(dplyr)

## ---- message=FALSE-----------------------------------------------------------
# build logistic regression model
m1 = glm(response ~ age + stage + grade, trial, family = binomial(link = "logit"))

# format results into data frame
tbl_regression(m1, exponentiate = TRUE)

## ---- message=FALSE-----------------------------------------------------------
# format results into data frame with global p-values
m1 %>%
  tbl_regression(
    exponentiate = TRUE, 
    pvalue_fun = function(x) style_pvalue(x, digits = 2),
    estimate_fun = function(x) style_ratio(x, digits = 3)
  ) %>% 
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>% 
  italicize_levels()

## -----------------------------------------------------------------------------
tbl_m1 <- tbl_regression(m1, exponentiate = TRUE)
tbl_m1

## -----------------------------------------------------------------------------
tbl_regression(m1) %>% names()

## -----------------------------------------------------------------------------
tbl_regression(m1) %>% purrr::pluck("gt_calls") %>% head(n = 5)

## ----as_gt2, eval=FALSE-------------------------------------------------------
#  tbl_regression(m1, exponentiate = TRUE) %>%
#    as_gt(exclude = "tab_footnote")

## ----as_gt1, echo=FALSE-------------------------------------------------------
# this code chunk only works if gt is installed
if (requireNamespace("gt", quietly = TRUE)) {
  tbl_regression(m1, exponentiate = TRUE) %>%
    as_gt(exclude = "tab_footnote")
}

## ----tbl_uvregression---------------------------------------------------------
trial %>%
  select(-death, -ttdeath, -stage) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = function(x) style_pvalue(x, digits = 2)
  ) %>%
  # overrides the default that shows p-values for each level
  add_global_p() %>%
  # adjusts global p-values for multiple testing (default method: FDR)
  add_q() %>%
  # bold p-values under a given threshold (default 0.05)
  bold_p() %>%
  # now bold q-values under the threshold of 0.10
  bold_p(t = 0.10, q = TRUE) %>%
  bold_labels()

## ---- eval=FALSE--------------------------------------------------------------
#  help("Rprofile")
#  
#  usethis::edit_r_profile()

## ---- echo=FALSE--------------------------------------------------------------
data.frame(
  `Description` = c("Formatting and rounding p-values", 
                    "Formatting and rounding for regression coefficients",
                    "Set level for limits",
                    "Print tables with `gt` or `kable`"),
  `Example` = c("`options(gtsummary.pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2))`",
                '`options(gtsummary.tbl_regression.estimate_fun = function(x) gtsummary::style_sigfig(x, digits = 3))`',
                '`options(gtsummary.conf.level = 0.90)`',
                '`options(gtsummary.print_engine = "kable")`   `options(gtsummary.print_engine = "gt")`'),
  `Functions` = c("`add_p()`, `tbl_regression()`, `tbl_uvregression()`",
                  "`tbl_regression()`, `tbl_uvregression()`",
                  "`tbl_regression()`, `tbl_uvregression()`",
                  "All `tbl_*()` functions")
) %>% 
  knitr::kable()

## ----eval=FALSE---------------------------------------------------------------
#  options(gtsummary.tbl_regression.estimate_fun = function(x) sigfig(x, digits = 3))

