## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)
library(gtsummary)

## -----------------------------------------------------------------------------
#  options(gtsummary.pvalue_fun = function(x) style_pvalue(x, digits = 2))

## -----------------------------------------------------------------------------
#  options(gtsummary.tbl_summary.percent_fun = function(x) style_number(x * 100, digits = 1))

## -----------------------------------------------------------------------------
#  options(gtsummary.print_engine = "kable")

## -----------------------------------------------------------------------------
#  options(gtsummary.print_engine = "gt")

## -----------------------------------------------------------------------------
#  options(gtsummary.as_gt.addl_cmds = "gt::tab_options(table.font.size = 'small', data_row.padding = gt::px(1))")

## ---- echo=FALSE, eval = TRUE-------------------------------------------------
tibble::tribble(
  ~`Option Name`, ~`Tests Modified`,
  "gtsummary.add_p.test.continuous_by2",           "continuous variables with 2-level by variable",
  "gtsummary.add_p.test.continuous",               "continuous variables with 3- or more level by variable",
  "gtsummary.add_p.test.categorical",              "categorical/dichotomous variables",
  "gtsummary.add_p.test.categorical.low_count",    "categorical/dichotomous variables with minimum expected count <5 in one cell",
  "gtsummary.add_p.test.categorical.group_by2",    "categorical/dichotomous grouped/correlated variables with 2-level by variable",
  "gtsummary.add_p.test.continuous.group_by2",     "continuous grouped/correlated variables with 2-level by variable"
) %>%
  gt::gt()

## -----------------------------------------------------------------------------
#  options(
#    gtsummary.add_p.test.continuous_by2 = "t.test",
#    gtsummary.add_p.test.continuous = "aov"
#  )

