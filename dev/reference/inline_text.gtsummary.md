# Report statistics from summary tables inline

Report statistics from summary tables inline

## Usage

``` r
# S3 method for class 'gtsummary'
inline_text(x, variable, level = NULL, column = NULL, pattern = NULL, ...)
```

## Arguments

- x:

  (`gtsummary`)  
  gtsummary object

- variable:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single variable name of statistic to present

- level:

  (`string`)  
  Level of the variable to display for categorical variables. Default is
  `NULL`

- column:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Column name to return from `x$table_body`.

- pattern:

  (`string`)  
  String indicating the statistics to return. Uses
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  formatting. Default is `NULL`

- ...:

  These dots are for future extensions and must be empty.

## Value

A string

## column + pattern

Some gtsummary tables report multiple statistics in a single cell, e.g.
`"{mean} ({sd})"` in
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
or
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
We often need to report just the mean or the SD, and that can be
accomplished by using both the `column=` and `pattern=` arguments. When
both of these arguments are specified, the column argument selects the
column to report statistics from, and the pattern argument specifies
which statistics to report, e.g.
`inline_text(x, column = "stat_1", pattern = "{mean}")` reports just the
mean from a
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
*This is not supported for all tables.*
