# Add differences between groups

Adds difference to tables created by
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
The difference between two groups (typically mean or rate difference) is
added to the table along with the difference's confidence interval and a
p-value (when applicable).

## Usage

``` r
# S3 method for class 'tbl_summary'
add_difference(
  x,
  test = NULL,
  group = NULL,
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

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  select(trt, age, marker, response, death) %>%
  tbl_summary(
    by = trt,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{p}%"
      ),
    missing = "no"
  ) |>
  add_n() |>
  add_difference()


  

Characteristic
```

**N**

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**Difference**²

**95% CI**²

**p-value**²

Age

189

47 (15)

47 (14)

-0.44

-4.6, 3.7

0.8

Marker Level (ng/mL)

190

1.02 (0.89)

0.82 (0.83)

0.20

-0.05, 0.44

0.12

Tumor Response

193

29%

34%

-4.2%

-18%, 9.9%

0.6

Patient Died

200

53%

59%

-5.8%

-21%, 9.0%

0.5

¹ Mean (SD); %

² Welch Two Sample t-test; 2-sample test for equality of proportions
with continuity correction

Abbreviation: CI = Confidence Interval

\# Example 2 ---------------------------------- \# ANCOVA adjusted for
grade and stage trial \|\>
[select](https://dplyr.tidyverse.org/reference/select.html)(trt, age,
marker, grade, stage)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(
by = trt, statistic =
[list](https://rdrr.io/r/base/list.html)([all_continuous](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "{mean} ({sd})"), missing = "no", include =
[c](https://rdrr.io/r/base/c.html)(age, marker, trt) ) \|\>
[add_n](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md)()
\|\>
[add_difference](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)(adj.vars
= [c](https://rdrr.io/r/base/c.html)(grade, stage))

[TABLE]
