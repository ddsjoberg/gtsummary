# Add column with N

For each variable in a `tbl_summary` table, the `add_n` function adds a
column with the total number of non-missing (or missing) observations

## Usage

``` r
# S3 method for class 'tbl_summary'
add_n(
  x,
  statistic = "{N_nonmiss}",
  col_label = "**N**",
  footnote = FALSE,
  last = FALSE,
  ...
)

# S3 method for class 'tbl_svysummary'
add_n(
  x,
  statistic = "{N_nonmiss}",
  col_label = "**N**",
  footnote = FALSE,
  last = FALSE,
  ...
)

# S3 method for class 'tbl_likert'
add_n(
  x,
  statistic = "{N_nonmiss}",
  col_label = "**N**",
  footnote = FALSE,
  last = FALSE,
  ...
)
```

## Arguments

- x:

  (`tbl_summary`)  
  Object with class `'tbl_summary'` created with
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  function.

- statistic:

  (`string`)  
  String indicating the statistic to report. Default is the number of
  non-missing observation for each variable,
  `statistic = "{N_nonmiss}"`. All statistics available to report
  include:

  - `"{N_obs}"` total number of observations,

  - `"{N_nonmiss}"` number of non-missing observations,

  - `"{N_miss}"` number of missing observations,

  - `"{p_nonmiss}"` percent non-missing data,

  - `"{p_miss}"` percent missing data

  The argument uses
  [`glue::glue()`](https://rdrr.io/pkg/glue/man/glue.html) syntax and
  multiple statistics may be reported, e.g.
  `statistic = "{N_nonmiss} / {N_obs} ({p_nonmiss}%)"`

- col_label:

  (`string`)  
  String indicating the column label. Default is `"**N**"`

- footnote:

  (scalar `logical`)  
  Logical argument indicating whether to print a footnote clarifying the
  statistics presented. Default is `FALSE`

- last:

  (scalar `logical`)  
  Logical indicator to include N column last in table. Default is
  `FALSE`, which will display N column first.

- ...:

  These dots are for future extensions and must be empty.

## Value

A table of class `c('tbl_summary', 'gtsummary')`

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
  add_n()


  

Characteristic
```
