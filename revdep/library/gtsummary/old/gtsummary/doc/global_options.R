## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## ----exit_early, include = FALSE, eval = !requireNamespace("gt")--------------
#  knitr::knit_exit()

## -----------------------------------------------------------------------------
#  options(gtsummary.pvalue_fun = function(x) style_pvalue(x, digits = 2))

## -----------------------------------------------------------------------------
#  options(gtsummary.pvalue_fun = purrr::partial(style_pvalue, digits = 2))

## -----------------------------------------------------------------------------
#  options(gtsummary.pvalue_fun = function(x) sprintf(x * 100, fmt='%#.1f'))

## -----------------------------------------------------------------------------
#  options(gtsummary.print_engine = "kable")

## -----------------------------------------------------------------------------
#  options(gtsummary.print_engine = "gt")

## ---- eval = TRUE-------------------------------------------------------------
library(gtsummary)
tbl_summary(trial)$gt_calls %>% head(n = 4)

## -----------------------------------------------------------------------------
#  options(gtsummary.as_gt.addl_cmds = "gt::tab_options(table.font.size = 'small', row.padding = gt::px(1))")

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

