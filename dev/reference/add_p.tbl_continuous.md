# Add p-values

Add p-values

## Usage

``` r
# S3 method for class 'tbl_continuous'
add_p(
  x,
  test = NULL,
  pvalue_fun = label_style_pvalue(digits = 1),
  include = everything(),
  test.args = NULL,
  group = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_continuous`)  
  table created with
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)

- test:

  List of formulas specifying statistical tests to perform for each
  variable. Default is two-way ANOVA when `by=` is not `NULL`, and has
  the same defaults as `add_p.tbl_continuous()` when `by = NULL`. See
  [tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
  for details, more tests, and instruction for implementing a custom
  test.

- pvalue_fun:

  (`function`)  
  Function to round and format p-values. Default is
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md).
  The function must have a numeric vector input, and return a string
  that is the rounded/formatted p-value (e.g.
  `pvalue_fun = label_style_pvalue(digits = 2)`).

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in output. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- test.args:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Containing additional arguments to pass to tests that accept
  arguments. For example, add an argument for all t-tests, use
  `test.args = all_tests("t.test") ~ list(var.equal = TRUE)`.

- group:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variable name of an ID or grouping variable. The column can be used to
  calculate p-values with correlated data. Default is `NULL`. See
  [tests](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
  for methods that utilize the `group` argument.

- ...:

  These dots are for future extensions and must be empty.

## Value

'tbl_continuous' object

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_continuous(variable = age, by = trt, include = grade) |>
  add_p(pvalue_fun = label_style_pvalue(digits = 2))


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**p-value**²

Grade

  

  

0.88

    I

46 (36, 60)

48 (42, 55)

  

    II

45 (31, 55)

51 (42, 58)

  

    III

52 (42, 61)

45 (36, 52)

  

¹ Age: Median (Q1, Q3)

² Two-way ANOVA

\# Example 2 ---------------------------------- trial \|\>
[tbl_continuous](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)(variable
= age, include = grade) \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)(test
= [everything](https://tidyselect.r-lib.org/reference/everything.html)()
~ "kruskal.test")

[TABLE]
