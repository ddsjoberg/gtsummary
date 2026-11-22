# Add p-values

Adds p-values to tables created by
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.md)
by comparing values across groups.

## Usage

``` r
# S3 method for class 'tbl_svysummary'
add_p(
  x,
  test = list(all_continuous() ~ "svy.wilcox.test", all_categorical() ~ "svy.chisq.test"),
  pvalue_fun = label_style_pvalue(digits = 1),
  include = everything(),
  test.args = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_svysummary`)  
  table created with
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.md)

- test:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  List of formulas specifying statistical tests to perform. Default is
  `list(all_continuous() ~ "svy.wilcox.test", all_categorical() ~ "svy.chisq.test")`.

  See below for details on default tests and
  [?tests](https://www.danieldsjoberg.com/gtsummary/reference/tests.md)
  for details on available tests and creating custom tests.

- pvalue_fun:

  (`function`)  
  Function to round and format p-values. Default is
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/reference/label_style.md).
  The function must have a numeric vector input, and return a string
  that is the rounded/formatted p-value (e.g.
  `pvalue_fun = label_style_pvalue(digits = 2)`).

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in output. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- test.args:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Containing additional arguments to pass to tests that accept
  arguments. For example, add an argument for all t-tests, use
  `test.args = all_tests("t.test") ~ list(var.equal = TRUE)`.

- ...:

  These dots are for future extensions and must be empty.

## Value

a gtsummary table of class `"tbl_svysummary"`

## Examples

``` r
# Example 1 ----------------------------------
# A simple weighted dataset
survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
  tbl_svysummary(by = Survived, include = c(Sex, Age)) |>
  add_p()
#> The following warnings were returned during `add_p()`:
#> ! For variable `Age` (`Survived`) and "statistic" and "p.value" statistics:
#>   Chi-squared approximation may be incorrect
#> ! For variable `Sex` (`Survived`) and "statistic" and "p.value" statistics:
#>   Chi-squared approximation may be incorrect


  

Characteristic
```
