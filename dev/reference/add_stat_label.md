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

  (`tbl_summary`)  
  Object with class `'tbl_summary'` or with class `'tbl_svysummary'`

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
  default statistic labels are used.

## Value

A `tbl_summary` or `tbl_svysummary` object

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
you need to match the `"label"` column values before the `"stat_column"`
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

**Drug A**  
N = 98

**Drug B**  
N = 102

Age, med. (iqr)

46 (37, 60)

48 (39, 56)

    Unknown

7

4

Grade, n (%)

  

  

    I

35 (36%)

33 (32%)

    II

32 (33%)

36 (35%)

    III

31 (32%)

33 (32%)

Tumor Response, n (%)

28 (29%)

33 (34%)

    Unknown

3

4

\# Example 2 ---------------------------------- tbl \|\> add_stat_label(
\# add a new column with statistic labels location = "column" )

[TABLE]

\# Example 3 ---------------------------------- trial \|\>
[select](https://dplyr.tidyverse.org/reference/select.html)(age, grade,
trt) \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(
by = trt, type =
[all_continuous](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "continuous2", statistic =
[all_continuous](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ [c](https://rdrr.io/r/base/c.html)("{median} ({p25}, {p75})", "{min} -
{max}"), ) \|\> add_stat_label(label = age ~
[c](https://rdrr.io/r/base/c.html)("IQR", "Range"))

[TABLE]
