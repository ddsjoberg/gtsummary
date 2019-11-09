## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE------------------------------------------------------
library(gtsummary)
library(dplyr)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("gtsummary")
#  remotes::install_github("rstudio/gt")
#  
#  library(gtsummary)
#  library(dplyr)

## ---- message=FALSE------------------------------------------------------
# build logistic regression model
m1 = glm(response ~ age + stage + grade, trial, family = binomial(link = "logit"))

# format results into data frame
tbl_regression(m1, exponentiate = TRUE)

## ---- message=FALSE------------------------------------------------------
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

## ------------------------------------------------------------------------
tbl_m1 <- tbl_regression(m1, exponentiate = TRUE)
tbl_m1

## ------------------------------------------------------------------------
tbl_regression(m1) %>% names()

## ------------------------------------------------------------------------
tbl_regression(m1) %>% purrr::pluck("gt_calls") %>% head(n = 5)

## ----as_gt2, eval=FALSE--------------------------------------------------
#  tbl_regression(m1, exponentiate = TRUE) %>%
#    as_gt(exclude = "footnote_abbreviation")

## ----as_gt1, echo=FALSE--------------------------------------------------
# this code chunk only works if gt is installed
if (requireNamespace("gt", quietly = TRUE)) {
  tbl_regression(m1, exponentiate = TRUE) %>%
    as_gt(exclude = "footnote_abbreviation")
}

## ----tbl_uvregression----------------------------------------------------
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

## ---- eval=FALSE---------------------------------------------------------
#  help("Rprofile")
#  
#  usethis::edit_r_profile()

## ----eval=FALSE----------------------------------------------------------
#  options(gtsummary.tbl_regression.estimate_fun = function(x) sigfig(x, digits = 3))

