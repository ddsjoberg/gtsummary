# Add p-values

Adds p-values to tables created by
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
by comparing values across groups.

## Usage

``` r
# S3 method for class 'tbl_summary'
add_p(
  x,
  test = NULL,
  pvalue_fun = label_style_pvalue(digits = 1),
  group = NULL,
  include = everything(),
  test.args = NULL,
  adj.vars = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_summary`)  
  table created with
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)

- test:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the statistical tests to perform for each variable, e.g.
  `list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test")`.

  See below for details on default tests and
  [?tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
  for details on available tests and creating custom tests.

- pvalue_fun:

  (`function`)  
  Function to round and format p-values. Default is
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md).
  The function must have a numeric vector input, and return a string
  that is the rounded/formatted p-value (e.g.
  `pvalue_fun = label_style_pvalue(digits = 2)`).

- group:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variable name of an ID or grouping variable. The column can be used to
  calculate p-values with correlated data. Default is `NULL`. See
  [tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
  for methods that utilize the `group` argument.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in output. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- test.args:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Containing additional arguments to pass to tests that accept
  arguments. For example, add an argument for all t-tests, use
  `test.args = all_tests("t.test") ~ list(var.equal = TRUE)`.

- adj.vars:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in adjusted calculations (e.g. in ANCOVA models).
  Default is `NULL`.

- ...:

  These dots are for future extensions and must be empty.

## Value

a gtsummary table of class `"tbl_summary"`

## test argument

See the
[?tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
help file for details on available tests and creating custom tests. The
[?tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
help file also includes pseudo-code for each test to be clear precisely
how the calculation is performed.

The default test used in
[`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
primarily depends on these factors:

- whether the variable is categorical/dichotomous vs continuous

- number of levels in the `tbl_summary(by)` variable

- whether the `add_p(group)` argument is specified

- whether the `add_p(adj.vars)` argument is specified

### Specified neither `add_p(group)` nor `add_p(adj.vars)`

- `"wilcox.test"` when `by` variable has two levels and variable is
  continuous.

- `"kruskal.test"` when `by` variable has more than two levels and
  variable is continuous.

- `"chisq.test.no.correct"` for categorical variables with all expected
  cell counts \>=5, and `"fisher.test"` for categorical variables with
  any expected cell count \<5.

### Specified `add_p(group)` and not `add_p(adj.vars)`

- `"lme4"` when `by` variable has two levels for all summary types.

*There is no default for grouped data when `by` variable has more than
two levels.* *Users must create custom tests for this scenario.*

### Specified `add_p(adj.vars)` and not `add_p(group)`

- `"ancova"` when variable is continuous and `by` variable has two
  levels.

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p()


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**p-value**²

Age

46 (37, 60)

48 (39, 56)

0.7

    Unknown

7

4

  

Grade

  

  

0.9

    I

35 (36%)

33 (32%)

  

    II

32 (33%)

36 (35%)

  

    III

31 (32%)

33 (32%)

  

¹ Median (Q1, Q3); n (%)

² Wilcoxon rank sum test; Pearson’s Chi-squared test

\# Example 2 ---------------------------------- trial \|\>
[select](https://dplyr.tidyverse.org/reference/select.html)(trt, age,
marker) \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(by
= trt, missing = "no") \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)(
\# perform t-test for all variables test =
[everything](https://tidyselect.r-lib.org/reference/everything.html)() ~
"t.test", \# assume equal variance in the t-test test.args =
[all_tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)("t.test")
~ [list](https://rdrr.io/r/base/list.html)(var.equal = TRUE) )

[TABLE]
