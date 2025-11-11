# Modify Column Merging

Merge two or more columns in a gtsummary table. Use
[`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
to print underlying column names.

## Usage

``` r
modify_column_merge(x, pattern, rows = NULL)

remove_column_merge(x, columns = everything())
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- pattern:

  (`string`)  
  glue syntax string indicating how to merge columns in `x$table_body`.
  For example, to construct a confidence interval use
  `"{conf.low}, {conf.high}"`.

- rows:

  (predicate `expression`)  
  Predicate expression to select rows in `x$table_body`. Review [rows
  argument
  details](https://www.danieldsjoberg.com/gtsummary/dev/reference/rows_argument.md).

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Selector of columns in `x$table_body`

## Value

gtsummary table

## Details

1.  Calling this function merely records the instructions to merge
    columns. The actual merging occurs when the gtsummary table is
    printed or converted with a function like
    [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md).

2.  Because the column merging is delayed, it is recommended to perform
    major modifications to the table, such as those with
    [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
    and
    [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md),
    before assigning merging instructions. Otherwise, unexpected
    formatting may occur in the final table.

3.  If this functionality is used in conjunction with
    [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
    (which includes
    [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)),
    there may be potential issues with printing. When columns are stack
    AND when the column-merging is defined with a quosure, you may run
    into issues due to the loss of the environment when 2 or more
    quosures are combined. If the expression version of the quosure is
    the same as the quosure (i.e. no evaluated objects), there should be
    no issues.

This function is used internally with care, and **it is *not*
recommended for users**.

## Future Updates

There are planned updates to the implementation of this function with
respect to the `pattern=` argument. Currently, this function replaces a
numeric column with a formatted character column following `pattern=`.
Once
[`gt::cols_merge()`](https://gt.rstudio.com/reference/cols_merge.html)
gains the `rows=` argument the implementation will be updated to use it,
which will keep numeric columns numeric. For the *vast majority* of
users, *the planned change will be go unnoticed*.

## See also

Other Advanced modifiers:
[`modify_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_indent.md),
[`modify_table_styling()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_styling.md)

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, missing = "no", include = c(age, marker, trt)) |>
  add_p(all_continuous() ~ "t.test", pvalue_fun = label_style_pvalue(prepend_p = TRUE)) |>
  modify_fmt_fun(statistic ~ label_style_sigfig()) |>
  modify_column_merge(pattern = "t = {statistic}; {p.value}") |>
  modify_header(statistic = "**t-test**")


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**t-test**²

Age

46 (37, 60)

48 (39, 56)

t = -0.21; p=0.8

Marker Level (ng/mL)

0.84 (0.23, 1.60)

0.52 (0.18, 1.21)

t = 1.6; p=0.12

¹ Median (Q1, Q3)

² Welch Two Sample t-test

\# Example 2 ----------------------------------
[lm](https://rdrr.io/r/stats/lm.html)(marker ~ age + grade, trial) \|\>
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)()
\|\> modify_column_merge( pattern = "{estimate} ({conf.low},
{conf.high})", rows = ![is.na](https://rdrr.io/r/base/NA.html)(estimate)
)

[TABLE]
