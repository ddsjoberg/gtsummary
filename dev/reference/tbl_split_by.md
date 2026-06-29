# Split gtsummary table by rows and/or columns

**\[experimental\]**  
The `tbl_split_by_rows()` and `tbl_split_by_columns()` functions split a
single gtsummary table into multiple tables. Both column-wise splitting
(that is, splits by columns in `x$table_body`) and row-wise splitting is
possible.

## Usage

``` r
tbl_split_by_rows(
  x,
  variables = NULL,
  row_numbers = NULL,
  variable_level = NULL,
  footnotes = c("all", "first", "last"),
  caption = c("all", "first", "last")
)

tbl_split_by_columns(
  x,
  keys,
  groups,
  footnotes = c("all", "first", "last"),
  caption = c("all", "first", "last")
)

# S3 method for class 'tbl_split'
print(x, ...)
```

## Arguments

- x:

  (`gtsummary` or `list`)  
  gtsummary table.

- variables, row_numbers, variable_level:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `integer`)  
  Specifies where the table will be split.

  - `variables`: Tables will be separated after each of the variables
    specified. The `x$table_body` data frame must contains a
    `'variable'` column to use this argument.

  - `row_numbers`: Row numbers after which the table will be split.

  - `variable_level`: A single column name in `x$table_body`. When
    specified, the table will be split at each unique level of the
    variable.

- footnotes, caption:

  (`string`) **\[experimental\]**  
  can be either `"first"`, `"all"`, or `"last"`, to locate global
  footnotes or caption only on the first, in each, or in the last table,
  respectively. It defaults to `"all"`. Reference footnotes are always
  present wherever they appear.

- keys:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  columns to be repeated in each table split. It defaults to the first
  column if missing (usually label column).

- groups:

  (list of `character` vectors)  
  list of column names that appear in `x$table_body`. Each group of
  column names represent a different table in the output list.

- ...:

  These dots are for future extensions and must be empty.

## Value

`tbl_split` object. If multiple splits are performed (e.g., both by row
and columns), the output is returned a single level list.

## Details

Run
[`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
to print all column names to split by.

Footnotes and caption handling are experimental and may change in the
future.

`row_numbers` indicates the row numbers at which to split the table. It
means that the table will be split after each of these row numbers. If
the last row is selected, the split will not happen as it is supposed to
happen after the last row.

To paginate gtsummary tables, you can use the [pager R
package](https://insightsengineering.github.io/pager/).

## Examples

``` r
# Example 1 ----------------------------------
# Split by rows
trial |>
  tbl_summary(by = trt) |>
  tbl_split_by_rows(variables = c(marker, grade)) |>
  dplyr::last() # Print only last table for simplicity


  

Characteristic
```
