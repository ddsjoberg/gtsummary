# Add differences rows between groups

**\[experimental\]**  
Adds difference to tables created by
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
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
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)

- reference:

  (scalar)  
  Value of the `tbl_summary(by)` variable value that is the reference
  for each of the difference calculations. For factors, use the
  character level.

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies summary statistics to display for each variable. The default
  is `everything() ~ "{estimate}"`. The statistics available to include
  will depend on the method specified in the `test` argument, but are
  generally `"estimate"`, `"std.error"`, `"parameter"`, `"statistic"`,
  `"conf.low"`, `"conf.high"`, `"p.value"`.

- test:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the tests/methods to perform for each variable, e.g.
  `list(all_continuous() ~ "t.test", all_dichotomous() ~ "prop.test", all_categorical(FALSE) ~ "smd")`.

  See below for details on default tests and
  [?tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
  for details on available tests and creating custom tests.

- group:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variable name of an ID or grouping variable. The column can be used to
  calculate p-values with correlated data. Default is `NULL`. See
  [tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
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

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
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
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md).
  The function must have a numeric vector input, and return a string
  that is the rounded/formatted p-value (e.g.
  `pvalue_fun = label_style_pvalue(digits = 2)`).

- estimate_fun:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  List of formulas specifying the functions to round and format
  differences and confidence limits.

- ...:

  These dots are for future extensions and must be empty.

## Value

a gtsummary table of class `"tbl_summary"`

## Details

The default labels for the statistic rows will often *not* be what you
need to display. In cases like this, use
[`modify_table_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_body.md)
to directly update the label rows. Use
[`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
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

**I**  
N = 68

**II**  
N = 68

**III**  
N = 64

Age, Mean (SD)

46 (15)

48 (14)

48 (14)

    Mean Difference

—

-1.4

-2.0

    95% CI

—

-6.4, 3.7

-7.1, 3.2

    p-value

—

0.6

0.5

Tumor Response, n (%)

21 (31%)

19 (30%)

21 (33%)

    Rate Difference

—

1.2%

-2.0%

    95% CI

—

-16%, 18%

-20%, 16%

    p-value

—

\>0.9

\>0.9

\# Example 2 ---------------------------------- \# Function to build
age-adjusted logistic regression and put results in ARD format
ard_odds_ratio \<- \\(data, variable, by, ...) {
cardx::[construct_model](https://insightsengineering.github.io/cardx/latest-tag/reference/construction_helpers.html)(
data = data, formula =
[reformulate](https://rdrr.io/r/stats/delete.response.html)(response =
variable, termlabels = [c](https://rdrr.io/r/base/c.html)(by, "age")),
\# adjusting model for age method = "glm", method.args =
[list](https://rdrr.io/r/base/list.html)(family = binomial) ) \|\>
cardx::[ard_regression_basic](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_regression_basic.html)(exponentiate
= TRUE) \|\>
dplyr::[filter](https://dplyr.tidyverse.org/reference/filter.html)(.data\$variable
== .env\$by) } trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(by
= trt, include = response, missing = "no") \|\>
[add_stat_label](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat_label.md)()
\|\>
[add_difference_row](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference_row.md)(
reference = "Drug A", statistic =
[everything](https://tidyselect.r-lib.org/reference/everything.html)() ~
[c](https://rdrr.io/r/base/c.html)("{estimate}", "{conf.low},
{conf.high}", "{p.value}"), test =
[everything](https://tidyselect.r-lib.org/reference/everything.html)() ~
ard_odds_ratio, estimate_fun =
[everything](https://tidyselect.r-lib.org/reference/everything.html)() ~
[label_style_ratio](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)()
) \|\> \# change the default label for the 'Odds Ratio'
[modify_table_body](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_body.md)(
~ .x \|\>
dplyr::[mutate](https://dplyr.tidyverse.org/reference/mutate.html)(
label = [ifelse](https://rdrr.io/r/base/ifelse.html)(label ==
"Coefficient", "Odds Ratio", label) ) ) \|\> \# add footnote about
logistic regression
[modify_footnote_body](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)(
footnote = "Age-adjusted logistic regression model", column = "label",
rows = variable == "response-row_difference" )

[TABLE]
