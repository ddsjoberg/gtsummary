# Report statistics from summary tables inline

Extracts and returns statistics from a
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
object for inline reporting in an R markdown document. Detailed examples
in the [inline_text
vignette](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)

## Usage

``` r
# S3 method for class 'tbl_summary'
inline_text(
  x,
  variable,
  column = NULL,
  level = NULL,
  pattern = NULL,
  pvalue_fun = label_style_pvalue(prepend_p = TRUE),
  ...
)

# S3 method for class 'tbl_svysummary'
inline_text(
  x,
  variable,
  column = NULL,
  level = NULL,
  pattern = NULL,
  pvalue_fun = label_style_pvalue(prepend_p = TRUE),
  ...
)
```

## Arguments

- x:

  (`tbl_summary`)  
  Object created from
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  or
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)

- variable:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single variable name of statistic to present

- column:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Column name to return from `x$table_body`. Can also pass the level of
  a by variable.

- level:

  (`string`)  
  Level of the variable to display for categorical variables. Default is
  `NULL`

- pattern:

  (`string`)  
  String indicating the statistics to return. Uses
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  formatting. Default is `NULL`

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
t1 <- trial |>
  tbl_summary(by = trt, include = grade) |>
  add_p()

inline_text(t1, variable = grade, level = "I", column = "Drug A", pattern = "{n}/{N} ({p}%)")
#> 35/98 (36%)
inline_text(t1, variable = grade, column = "p.value")
#> [1] "p=0.9"
```
