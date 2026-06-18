# Modify hidden columns

Use these functions to hide or unhide columns in a gtsummary table. Use
`show_header_names(show_hidden=TRUE)` to print available columns to
update.

## Usage

``` r
modify_column_hide(x, columns)

modify_column_unhide(x, columns)
```

## Arguments

- x:

  (`gtsummary`)  
  gtsummary object

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Selector of columns in `x$table_body`

## Author

Daniel D. Sjoberg

## Examples

``` r

# Example 1 ----------------------------------
# hide 95% CI, and replace with standard error
lm(age ~ marker + grade, trial) |>
  tbl_regression() |>
  modify_column_hide(conf.low) |>
  modify_column_unhide(columns = std.error)


  

Characteristic
```
