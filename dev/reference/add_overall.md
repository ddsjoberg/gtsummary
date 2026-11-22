# Add overall column

Adds a column with overall summary statistics to tables created by
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md),
[`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
or
[`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md).

## Usage

``` r
add_overall(x, ...)

# S3 method for class 'tbl_summary'
add_overall(
  x,
  last = FALSE,
  col_label = "**Overall**  \nN = {style_number(N)}",
  statistic = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'tbl_continuous'
add_overall(
  x,
  last = FALSE,
  col_label = "**Overall**  \nN = {style_number(N)}",
  statistic = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'tbl_svysummary'
add_overall(
  x,
  last = FALSE,
  col_label = "**Overall**  \nN = {style_number(N)}",
  statistic = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'tbl_custom_summary'
add_overall(
  x,
  last = FALSE,
  col_label = "**Overall**  \nN = {style_number(N)}",
  statistic = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'tbl_hierarchical'
add_overall(
  x,
  last = FALSE,
  col_label = "**Overall**  \nN = {style_number(N)}",
  statistic = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'tbl_hierarchical_count'
add_overall(
  x,
  last = FALSE,
  col_label = ifelse(rlang::is_empty(x$inputs$denominator), "**Overall**",
    "**Overall**  \nN = {style_number(N)}"),
  statistic = NULL,
  digits = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_summary`, `tbl_svysummary`, `tbl_continuous`,
  `tbl_custom_summary`)  
  A stratified 'gtsummary' table

- ...:

  These dots are for future extensions and must be empty.

- last:

  (scalar `logical`)  
  Logical indicator to display overall column last in table. Default is
  `FALSE`, which will display overall column first.

- col_label:

  (`string`)  
  String indicating the column label. Default is
  `"**Overall** \nN = {style_number(N)}"`

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Override the statistic argument in initial `tbl_*` function call.
  Default is `NULL`.

- digits:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Override the digits argument in initial `tbl_*` function call. Default
  is `NULL`.

## Value

A `gtsummary` of same class as `x`

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(include = c(age, grade), by = trt) |>
  add_overall()


  

Characteristic
```
