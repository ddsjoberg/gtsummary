# Modify column indentation

Add, increase, or reduce indentation for columns.

## Usage

``` r
modify_indent(x, columns, rows = NULL, indent = 4L, double_indent, undo)
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

- indent:

  (`integer`)  
  An integer indicating how many space to indent text

- double_indent, undo:

  **\[deprecated\]**

## Value

a gtsummary table

## See also

Other Advanced modifiers:
[`modify_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md),
[`modify_table_styling()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_styling.md)

## Examples

``` r
# remove indentation from `tbl_summary()`
trial |>
  tbl_summary(include = grade) |>
  modify_indent(columns = label, indent = 0L)


  

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

\# increase indentation in \`tbl_summary\` trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(include
= grade) \|\> modify_indent(columns = label, rows = !row_type
[%in%](https://rdrr.io/r/base/match.html) 'label', indent = 8L)

[TABLE]
