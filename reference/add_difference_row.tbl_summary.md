# Add differences rows between groups

**\[experimental\]**  
Adds difference to tables created by
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.md)
as additional rows. This function is often useful when there are more
than two groups to compare.

Pairwise differences are calculated relative to the specified `by`
variable's specified reference level.

## Usage

``` r
# S3 method for class 'tbl_summary'
add_difference_row(
  x,
  reference,
  statistic = everything() ~ "{estimate}",
  test = NULL,
  group = NULL,
  header = NULL,
  adj.vars = NULL,
  test.args = NULL,
  conf.level = 0.95,
  include = everything(),
  pvalue_fun = label_style_pvalue(digits = 1),
  estimate_fun = list(c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig(),
    all_dichotomous() ~ label_style_sigfig(scale = 100, suffix = "%"), all_tests("smd")
    ~ label_style_sigfig()),
  ...
)
```

## Arguments

- x:

  (`tbl_summary`)  
  table created with
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.md)

- reference:

  (scalar)  
  Value of the `tbl_summary(by)` variable value that is the reference
  for each of the difference calculations. For factors, use the
  character level.

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Specifies summary statistics to display for each variable. The default
  is `everything() ~ "{estimate}"`. The statistics available to include
  will depend on the method specified in the `test` argument, but are
  generally `"estimate"`, `"std.error"`, `"parameter"`, `"statistic"`,
  `"conf.low"`, `"conf.high"`, `"p.value"`.

- test:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Specifies the tests/methods to perform for each variable, e.g.
  `list(all_continuous() ~ "t.test", all_dichotomous() ~ "prop.test", all_categorical(FALSE) ~ "smd")`.

  See below for details on default tests and
  [?tests](https://www.danieldsjoberg.com/gtsummary/reference/tests.md)
  for details on available tests and creating custom tests.

- group:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variable name of an ID or grouping variable. The column can be used to
  calculate p-values with correlated data. Default is `NULL`. See
  [tests](https://www.danieldsjoberg.com/gtsummary/reference/tests.md)
  for methods that utilize the `group` argument.

- header:

  (`string`)  
  When supplied, a header row will appear above the difference
  statistics.

- adj.vars:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in adjusted calculations (e.g. in ANCOVA models).
  Default is `NULL`.

- test.args:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Containing additional arguments to pass to tests that accept
  arguments. For example, add an argument for all t-tests, use
  `test.args = all_tests("t.test") ~ list(var.equal = TRUE)`.

- conf.level:

  (`numeric`)  
  a scalar in the interval `(0, 1)` indicating the confidence level.
  Default is 0.95

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in output. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- pvalue_fun:

  (`function`)  
  Function to round and format p-values. Default is
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/reference/label_style.md).
  The function must have a numeric vector input, and return a string
  that is the rounded/formatted p-value (e.g.
  `pvalue_fun = label_style_pvalue(digits = 2)`).

- estimate_fun:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  List of formulas specifying the functions to round and format
  differences and confidence limits.

- ...:

  These dots are for future extensions and must be empty.

## Value

a gtsummary table of class `"tbl_summary"`

## Details

The default labels for the statistic rows will often *not* be what you
need to display. In cases like this, use
[`modify_table_body()`](https://www.danieldsjoberg.com/gtsummary/reference/modify_table_body.md)
to directly update the label rows. Use
[`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.md)
to print the underlying column names to identify the columns to target
when changing the label, which in this case will always be the `'label'`
column. See Example 2.

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(
    by = grade,
    include = c(age, response),
    missing = "no",
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) |>
  add_stat_label() |>
  add_difference_row(
    reference = "I",
    statistic = everything() ~ c("{estimate}", "{conf.low}, {conf.high}", "{p.value}")
  )


  

Characteristic
```
