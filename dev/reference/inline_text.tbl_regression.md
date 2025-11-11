# Report statistics from regression summary tables inline

Takes an object with class `tbl_regression`, and the location of the
statistic to report and returns statistics for reporting inline in an R
markdown document. Detailed examples in the [inline_text
vignette](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)

## Usage

``` r
# S3 method for class 'tbl_regression'
inline_text(
  x,
  variable,
  level = NULL,
  pattern = "{estimate} ({conf.level*100}% CI {conf.low}, {conf.high}; {p.value})",
  estimate_fun = x$inputs$estimate_fun,
  pvalue_fun = label_style_pvalue(prepend_p = TRUE),
  ...
)
```

## Arguments

- x:

  (`tbl_regression`)  
  Object created by
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)

- variable:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single variable name of statistic to present

- level:

  (`string`)  
  Level of the variable to display for categorical variables. Default is
  `NULL`

- pattern:

  (`string`)  
  String indicating the statistics to return. Uses
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  formatting. Default is
  `"{estimate} ({conf.level }\% CI {conf.low}, {conf.high}; {p.value})"`.
  All columns from `x$table_body` are available to print as well as the
  confidence level (`conf.level`). See below for details.

- estimate_fun:

  (`function`)  
  Function to style model coefficient estimates. Columns `'estimate'`,
  `'conf.low'`, and `'conf.high'` are formatted. Default is
  `x$inputs$estimate_fun`

- pvalue_fun:

  function to style p-values and/or q-values. Default is
  `label_style_pvalue(prepend_p = TRUE)`

- ...:

  These dots are for future extensions and must be empty.

## Value

A string reporting results from a gtsummary table

## pattern argument

The following items (and more) are available to print. Use
`print(x$table_body)` to print the table the estimates are extracted
from.

- `{estimate}` coefficient estimate formatted with 'estimate_fun'

- `{conf.low}` lower limit of confidence interval formatted with
  'estimate_fun'

- `{conf.high}` upper limit of confidence interval formatted with
  'estimate_fun'

- `{p.value}` p-value formatted with 'pvalue_fun'

- `{N}` number of observations in model

- `{label}` variable/variable level label

## Author

Daniel D. Sjoberg

## Examples

``` r
inline_text_ex1 <-
  glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE)

inline_text(inline_text_ex1, variable = age)
#> [1] "1.02 (95% CI 1.00, 1.04; p=0.10)"
inline_text(inline_text_ex1, variable = grade, level = "III")
#> [1] "1.01 (95% CI 0.47, 2.16; p>0.9)"
```
