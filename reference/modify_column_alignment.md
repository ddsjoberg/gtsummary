# Modify column alignment

Update column alignment/justification in a gtsummary table.

## Usage

``` r
modify_column_alignment(x, columns, align = c("left", "right", "center"))
```

## Arguments

- x:

  (`gtsummary`)  
  gtsummary object

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Selector of columns in `x$table_body`

- align:

  (`string`)  
  String indicating alignment of column, must be one of
  `c("left", "right", "center")`

## Examples

``` r
# Example 1 ----------------------------------
lm(age ~ marker + grade, trial) %>%
  tbl_regression() %>%
  modify_column_alignment(columns = everything(), align = "left")


  

Characteristic
```

**Beta**

**95% CI**

**p-value**

Marker Level (ng/mL)

-0.04

-2.6, 2.5

\>0.9

Grade

  

  

  

    I

—

—

  

    II

0.64

-4.7, 6.0

0.8

    III

2.4

-2.8, 7.6

0.4

Abbreviation: CI = Confidence Interval
