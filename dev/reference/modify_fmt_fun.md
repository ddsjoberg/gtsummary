# Modify formatting functions

Use this function to update the way numeric columns and rows of
`.$table_body` are formatted

## Usage

``` r
modify_fmt_fun(x, ..., rows = NULL, update, quiet)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- ...:

  [`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)  
  Used to assign updates to formatting functions.

  Use `modify_fmt_fun(colname = <fmt fun>)` to update a single column.
  Using a formula will invoke tidyselect, e.g.
  `modify_fmt_fun(c(estimate, conf.low, conf.high) ~ <fmt_fun>)`.

  Use the
  [`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  to see the column names that can be modified.

- rows:

  (predicate `expression`)  
  Predicate expression to select rows in `x$table_body`. Can be used to
  style footnote, formatting functions, missing symbols, and text
  formatting. Default is `NULL`. See details below.

- update, quiet:

  **\[deprecated\]**

## rows argument

The rows argument accepts a predicate expression that is used to specify
rows to apply formatting. The expression must evaluate to a logical when
evaluated in `x$table_body`. For example, to apply formatting to the age
rows pass `rows = variable == "age"`. A vector of row numbers is NOT
acceptable.

A couple of things to note when using the `rows` argument.

1.  You can use saved objects to create the predicate argument, e.g.
    `rows = variable == letters[1]`.

2.  The saved object cannot share a name with a column in
    `x$table_body`. The reason for this is that in
    [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
    the columns are renamed, and the renaming process cannot
    disambiguate the `variable` column from an external object named
    `variable` in the following expression
    `rows = .data$variable = .env$variable`.

## Examples

``` r
# Example 1 ----------------------------------
# show 'grade' p-values to 3 decimal places and estimates to 4 sig figs
lm(age ~ marker + grade, trial) |>
  tbl_regression() %>%
  modify_fmt_fun(
    p.value = label_style_pvalue(digits = 3),
    c(estimate, conf.low, conf.high) ~ label_style_sigfig(digits = 4),
    rows = variable == "grade"
  )


  

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

0.6365

-4.684, 5.957

0.814

    III

2.394

-2.822, 7.610

0.366

Abbreviation: CI = Confidence Interval
