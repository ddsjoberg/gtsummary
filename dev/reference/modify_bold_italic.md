# Modify Bold and Italic

Add or remove bold and italic styling to a cell in a table. By default,
the remove functions will remove all bold/italic styling.

## Usage

``` r
modify_bold(x, columns, rows)

remove_bold(x, columns = everything(), rows = TRUE)

modify_italic(x, columns, rows)

remove_italic(x, columns = everything(), rows = TRUE)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Selector of columns in `x$table_body`

- rows:

  (predicate `expression`)  
  Predicate expression to select rows in `x$table_body`. Review [rows
  argument
  details](https://www.danieldsjoberg.com/gtsummary/dev/reference/rows_argument.md).

## Value

Updated gtsummary object

## Examples

``` r
# Example 1 ----------------------------------
tbl <- trial |>
  tbl_summary(include = grade) |>
  modify_bold(columns = label, rows = row_type == "label") |>
  modify_italic(columns = label, rows = row_type == "level")
tbl


  

Characteristic
```

**N = 200**¹

Grade

  

    I

68 (34%)

    II

68 (34%)

    III

64 (32%)

¹ n (%)

\# Example 2 ---------------------------------- tbl \|\>
remove_bold(columns = label, rows = row_type == "label") \|\>
remove_italic(columns = label, rows = row_type == "level")

[TABLE]
