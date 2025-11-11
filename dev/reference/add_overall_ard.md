# ARD add overall column

Adds a column with overall summary statistics to tables created by
[`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md).

## Usage

``` r
# S3 method for class 'tbl_ard_summary'
add_overall(
  x,
  cards,
  last = FALSE,
  col_label = "**Overall**",
  statistic = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_ard_summary`)  
  A stratified 'gtsummary' table

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- last:

  (scalar `logical`)  
  Logical indicator to display overall column last in table. Default is
  `FALSE`, which will display overall column first.

- col_label:

  (`string`)  
  String indicating the column label. Default is `"**Overall**"`

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Override the statistic argument in initial `tbl_*` function call.
  Default is `NULL`.

- ...:

  These dots are for future extensions and must be empty.

## Value

A `gtsummary` of same class as `x`

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
# build primary table
tbl <-
  cards::ard_stack(
    trial,
    .by = trt,
    cards::ard_summary(variables = age),
    cards::ard_tabulate(variables = grade),
    .missing = TRUE,
    .attributes = TRUE,
    .total_n = TRUE
  ) |>
  tbl_ard_summary(by = trt)

# create ARD with overall results
ard_overall <-
  cards::ard_stack(
    trial,
    cards::ard_summary(variables = age),
    cards::ard_tabulate(variables = grade),
    .missing = TRUE,
    .attributes = TRUE,
    .total_n = TRUE
  )

# add an overall column
tbl |>
  add_overall(cards = ard_overall)


  

Characteristic
```

**Overall**¹

**Drug A**¹

**Drug B**¹

Age

47.0 (38.0, 57.0)

46.0 (37.0, 60.0)

48.0 (39.0, 56.0)

Grade

  

  

  

    I

68 (34.0%)

35 (35.7%)

33 (32.4%)

    II

68 (34.0%)

32 (32.7%)

36 (35.3%)

    III

64 (32.0%)

31 (31.6%)

33 (32.4%)

¹ Median (Q1, Q3); n (%)
