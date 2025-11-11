# Modify Missing Substitution

Specify how missing values will be represented in the printed table. By
default, a blank space is printed for all `NA` values.

## Usage

``` r
modify_missing_symbol(x, symbol, columns, rows)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- symbol:

  (`string`)  
  string indicating how missing values are formatted.

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  columns to add missing symbol.

- rows:

  (predicate `expression`)  
  Predicate expression to select rows in `x$table_body`. Review [rows
  argument
  details](https://www.danieldsjoberg.com/gtsummary/dev/reference/rows_argument.md).

## Value

Updated gtsummary object

## Examples

``` r
# Use the abbreivation "Ref." for reference rows instead of the em-dash
lm(marker ~ trt, data = trial) |>
  tbl_regression() |>
  modify_missing_symbol(
    symbol = "Ref.",
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row == TRUE
  )


  

Characteristic
```

**Beta**

**95% CI**

**p-value**

Chemotherapy Treatment

  

  

  

    Drug A

Ref.

Ref.

  

    Drug B

-0.20

-0.44, 0.05

0.12

Abbreviation: CI = Confidence Interval
