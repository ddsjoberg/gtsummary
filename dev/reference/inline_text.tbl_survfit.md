# Report statistics from survfit tables inline

Extracts and returns statistics from a `tbl_survfit` object for inline
reporting in an R markdown document. Detailed examples in the
[inline_text
vignette](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)

## Usage

``` r
# S3 method for class 'tbl_survfit'
inline_text(
  x,
  variable = NULL,
  level = NULL,
  pattern = NULL,
  time = NULL,
  prob = NULL,
  column = NULL,
  estimate_fun = x$inputs$estimate_fun,
  pvalue_fun = label_style_pvalue(prepend_p = TRUE),
  ...
)
```

## Arguments

- x:

  (`tbl_survfit`)  
  Object created from
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)

- variable:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variable name of statistic to present.

- level:

  (`string`)  
  Level of the variable to display for categorical variables. Can also
  specify the 'Unknown' row. Default is `NULL`

- pattern:

  (`string`)  
  String indicating the statistics to return.

- time, prob:

  (`numeric` scalar)  
  time or probability for which to return result

- column:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  column to print from `x$table_body`. Columns may be selected with
  `time` or `prob` arguments as well.

- estimate_fun:

  (`function`)  
  Function to round and format estimate and confidence limits. Default
  is the same function used in
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)

- pvalue_fun:

  (`function`)  
  Function to round and format p-values. Default is
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md).
  The function must have a numeric vector input, and return a string
  that is the rounded/formatted p-value (e.g.
  `pvalue_fun = label_style_pvalue(digits = 2)`).

- ...:

  These dots are for future extensions and must be empty.

## Value

A string reporting results from a gtsummary table

## Author

Daniel D. Sjoberg

## Examples

``` r
library(survival)

# fit survfit
fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)

# sumarize survfit objects
tbl1 <-
  tbl_survfit(
    fit1,
    times = c(12, 24),
    label = ~"Treatment",
    label_header = "**{time} Month**"
  ) %>%
  add_p()

tbl2 <-
  tbl_survfit(
    fit2,
    probs = 0.5,
    label_header = "**Median Survival**"
  )

# report results inline
inline_text(tbl1, time = 24, level = "Drug B")
#> [1] "41% (33%, 52%)"
inline_text(tbl1, time = 24, level = "Drug B",
            pattern = "{estimate} [95% CI {conf.low}, {conf.high}]")
#> 41% [95% CI 33%, 52%]
inline_text(tbl1, column = p.value)
#> [1] "p=0.2"
inline_text(tbl2, prob = 0.5)
#> [1] "22 (21, —)"
```
