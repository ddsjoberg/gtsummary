# Summarize a continuous variable

**\[deprecated\]**  
This helper, to be used with
[`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md),
creates a function summarizing a continuous variable.

## Usage

``` r
continuous_summary(variable)
```

## Arguments

- variable:

  (`string`)  
  String indicating the name of the variable to be summarized. This
  variable should be continuous.

## Details

When using `continuous_summary()`, you can specify in the `statistic=`
argument of
[`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md)
the same continuous statistics than in
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
See the *statistic argument* section of the help file of
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).

## Author

Joseph Larmarange
