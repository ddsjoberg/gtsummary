# Add p-values

Adds p-values to tables created by
[`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
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
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)

- test:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  List of formulas specifying statistical tests to perform. Default is
  `list(all_continuous() ~ "svy.wilcox.test", all_categorical() ~ "svy.chisq.test")`.

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

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in output. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- test.args:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
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

**No**  
N = 1,490¹

**Yes**  
N = 711¹

**p-value**²

Sex

  

  

0.048

    Male

1,364 (92%)

367 (52%)

  

    Female

126 (8.5%)

344 (48%)

  

Age

  

  

0.4

    Child

52 (3.5%)

57 (8.0%)

  

    Adult

1,438 (97%)

654 (92%)

  

¹ n (%)

² Pearson’s X^2: Rao & Scott adjustment

\# A dataset with a complex design
[data](https://rdrr.io/r/utils/data.html)(api, package = "survey")
d_clust \<-
survey::[svydesign](https://rdrr.io/pkg/survey/man/svydesign.html)(id =
~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) \# Example 2
----------------------------------
[tbl_svysummary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)(d_clust,
by = both, include = [c](https://rdrr.io/r/base/c.html)(api00, api99))
\|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)()

[TABLE]

\# Example 3 ---------------------------------- \# change tests to svy
t-test and Wald test
[tbl_svysummary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)(d_clust,
by = both, include = [c](https://rdrr.io/r/base/c.html)(api00, api99,
stype)) \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)(
test = [list](https://rdrr.io/r/base/list.html)(
[all_continuous](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "svy.t.test",
[all_categorical](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "svy.wald.test" ) )

[TABLE]
