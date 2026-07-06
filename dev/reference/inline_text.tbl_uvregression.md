# Report statistics from regression summary tables inline

Extracts and returns statistics from a table created by the
`tbl_uvregression` function for inline reporting in an R markdown
document. Detailed examples in the [inline_text
vignette](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)

## Usage

``` r
# S3 method for class 'tbl_uvregression'
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

  (`tbl_uvregression`)  
  Object created by
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)

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
  formatting. Default is `NULL`

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
  trial[c("response", "age", "grade")] %>%
  tbl_uvregression(
    method = glm,
    method.args = list(family = binomial),
    y = response,
    exponentiate = TRUE
  )

inline_text(inline_text_ex1, variable = age)
#> [1] "1.02 (95% CI 1.00, 1.04; p=0.10)"
inline_text(inline_text_ex1, variable = grade, level = "III")
#> [1] "1.10 (95% CI 0.52, 2.29; p=0.8)"
```
