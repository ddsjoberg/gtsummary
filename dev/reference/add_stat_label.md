# Add statistic labels

**\[questioning\]**  
Adds or modifies labels describing the summary statistics presented for
each variable in a
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
table.

## Usage

``` r
add_stat_label(x, ...)

# S3 method for class 'tbl_summary'
add_stat_label(x, location = c("row", "column"), label = NULL, ...)

# S3 method for class 'tbl_svysummary'
add_stat_label(x, location = c("row", "column"), label = NULL, ...)

# S3 method for class 'tbl_ard_summary'
add_stat_label(x, location = c("row", "column"), label = NULL, ...)
```

## Arguments

- x:

  (`tbl_summary`, `tbl_svysummary`, `tbl_ard_summary`)  
  Object with class `'tbl_summary'`, `'tbl_svysummary'`, or
  `'tbl_ard_summary'`.

- ...:

  These dots are for future extensions and must be empty.

- location:

  (`string`)  
  Location where statistic label will be included. `"row"` (the default)
  to add the statistic label to the variable label row, and `"column"`
  adds a column with the statistic label.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  indicates the updates to the statistic label, e.g.
  `label = all_categorical() ~ "No. (%)"`. When not specified, the
  default statistic labels are used. The default labels are constructed
  from the `statistic` argument, e.g. `"{median} ({p25}, {p75})"` is
  labelled `"Median (Q1, Q3)"`. Use `NA_character_` to suppress a
  variable's statistic label. See the continuous2 section below for
  `'continuous2'` variables.

## Value

A `tbl_summary`, `tbl_svysummary`, or `tbl_ard_summary` object

## continuous2 variables

Variables summarized as `'continuous2'` already present each statistic
on its own row, and the statistic labels appear in the `"label"` column.
Updates to these labels replace the text in those rows.

- Pass an unnamed character vector the same length as the variable's
  `statistic` argument to replace all the labels, e.g.
  `label = age ~ c("IQR", "Range")`.

- Pass a named character vector to update a subset of the labels, where
  the names are the statistic strings passed in
  `tbl_summary(statistic)`, e.g.
  `label = age ~ c("{min} - {max}" = "Range")`.

To change the text of the missing row, use the `missing_text` argument
of
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).

`add_stat_label()` can only be applied to a table once. Subsequent calls
return the table unaltered.

## Tips

When using `add_stat_label(location='row')` with subsequent
[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md),
it's important to have somewhat of an understanding of the underlying
structure of the gtsummary table. `add_stat_label(location='row')` works
by adding a new column called `"stat_label"` to `x$table_body`. The
`"label"` and `"stat_label"` columns are merged when the gtsummary table
is printed. The
[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
function merges on the `"label"` column (among others), which is
typically the first column you see in a gtsummary table. Therefore, when
you want to merge a table that has run `add_stat_label(location='row')`
you need to match the `"label"` column values before the `"stat_label"`
is merged with it.

For example, the following two tables merge properly

    tbl1 <- trial %>% select(age, grade) |> tbl_summary() |> add_stat_label()
    tbl2 <- lm(marker ~ age + grade, trial) |> tbl_regression()

    tbl_merge(list(tbl1, tbl2))

The addition of the new `"stat_label"` column requires a default labels
for categorical variables, which is `"No. (%)"`. This can be changed to
either desired text or left blank using `NA_character_`. The blank
option is useful in the `location="row"` case to keep the output for
categorical variables identical what was produced without a
`"add_stat_label()"` function call.

## Author

Daniel D. Sjoberg

## Examples

``` r
tbl <- trial |>
  dplyr::select(trt, age, grade, response) |>
  tbl_summary(by = trt)

# Example 1 ----------------------------------
# Add statistic presented to the variable label row
tbl |>
  add_stat_label(
    # update default statistic label for continuous variables
    label = all_continuous() ~ "med. (iqr)"
  )


  

Characteristic
```
