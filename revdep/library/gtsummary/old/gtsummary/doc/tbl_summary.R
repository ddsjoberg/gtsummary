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
# printing trial data
head(trial) %>% knitr::kable()

## ---- message=FALSE-----------------------------------------------------------
trial2 =
  trial %>%
  select(trt, marker, stage)

tbl_summary(trial2)

## -----------------------------------------------------------------------------
tbl_summary(trial2, by = trt) %>% add_p()

## -----------------------------------------------------------------------------
trial2 %>%
  # build base summary table
  tbl_summary(
    by = trt,
    # change variable labels
    label = list(vars(marker) ~ "Marker, ng/mL",
                 vars(stage) ~ "Clinical T Stage"),
    # change statistics printed in table
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = list("marker" ~ c(1, 2))
  ) %>%
  # add p-values, report t-test, round large pvalues to two decimal place
  add_p(test = list(vars(marker) ~ "t.test"),
                 pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  # add statistic labels
  add_stat_label() %>%
  # bold variable labels, italicize levels
  bold_labels() %>%
  italicize_levels() %>%
  # bold p-values under a given threshold (default is 0.05)
  bold_p(t = 0.2) %>%
  # include percent in headers
  modify_header(stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")

## -----------------------------------------------------------------------------
trial %>%
  dplyr::select(trt, response, age, stage, marker, grade) %>%
  tbl_summary(
    by = trt,
    type = list(c("response", "grade") ~ "categorical"), # select by variables in a vector
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{p}%") # select by summary type/attribute
  ) %>%
  add_p(test = list(contains("response") ~ "fisher.test", # select using functions in tidyselect
                    all_continuous() ~ "t.test"))

## -----------------------------------------------------------------------------
tab1 = tbl_summary(trial2, by = trt)
tab1

## -----------------------------------------------------------------------------
tbl_summary(trial2) %>% names()

## -----------------------------------------------------------------------------
tbl_summary(trial2) %>% purrr::pluck("gt_calls") %>% head(n = 5)

## ----as_gt2, eval=FALSE-------------------------------------------------------
#  tbl_summary(trial2, by = trt) %>%
#    as_gt(exclude = "tab_footnote") %>%
#    gt::tab_spanner(label = gt::md("**Treatment Group**"),
#                    columns = gt::starts_with("stat_"))

## ----as_gt1, echo=FALSE-------------------------------------------------------
# this code chunk only works if gt is installed
if (requireNamespace("gt", quietly = TRUE)) {
  tbl_summary(trial2, by = trt) %>%
    as_gt(exclude = "tab_footnote") %>%
    gt::tab_spanner(label = gt::md("**Treatment Group**"),
                    columns = gt::starts_with("stat_"))
}

## ---- eval=FALSE--------------------------------------------------------------
#  help("Rprofile")
#  
#  usethis::edit_r_profile()

## ---- echo=FALSE--------------------------------------------------------------
data.frame(
  `Description` = c("Formatting and rounding p-values", 
                    "Formatting and rounding percentages",
                    "Print tables with `gt` or `kable`"),
  `Example` = c("`options(gtsummary.pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2))`",
                '`options(gtsummary.tbl_summary.percent_fun = function(x) sprintf("%.2f", 100 * x))`',
                '`options(gtsummary.print_engine = "kable")`   `options(gtsummary.print_engine = "gt")`'),
  `Functions` = c("`add_p()`, `tbl_regression()`, `tbl_uvregression()`",
                  "`tbl_summary()`",
                  "All `tbl_*()` functions")
) %>% 
  knitr::kable()

## ----eval=FALSE---------------------------------------------------------------
#  options(gtsummary.tbl_summary.percent_fun = function(x) sigfig(x, digits = 3))

