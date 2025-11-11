# Add p-value

Calculate and add a p-value to stratified
[`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
tables.

## Usage

``` r
# S3 method for class 'tbl_survfit'
add_p(
  x,
  test = "logrank",
  test.args = NULL,
  pvalue_fun = label_style_pvalue(digits = 1),
  include = everything(),
  quiet,
  ...
)
```

## Arguments

- x:

  (`tbl_survfit`)  
  Object of class `"tbl_survfit"`

- test:

  (`string`)  
  string indicating test to use. Must be one of `"logrank"`, `"tarone"`,
  `"survdiff"`, `"petopeto_gehanwilcoxon"`, `"coxph_lrt"`,
  `"coxph_wald"`, `"coxph_score"`. See details below

- test.args:

  (named `list`)  
  named list of arguments that will be passed to the method specified in
  the `test` argument. Default is `NULL`.

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

- quiet:

  **\[deprecated\]**

- ...:

  These dots are for future extensions and must be empty.

## test argument

The most common way to specify `test=` is by using a single string
indicating the test name. However, if you need to specify different
tests within the same table, the input in flexible using the list
notation common throughout the gtsummary package. For example, the
following code would call the log-rank test, and a second test of the
*G-rho* family.

    ... |>
      add_p(test = list(trt ~ "logrank", grade ~ "survdiff"),
            test.args = grade ~ list(rho = 0.5))

## Note

To calculate the p-values, the formula is re-constructed from the the
call in the original
[`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html) object.
When the [`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
object is created a for loop,
[`lapply()`](https://rdrr.io/r/base/lapply.html),
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) setting
the call *may not* reflect the true formula which may result in an error
or an incorrect calculation.

To ensure correct results, the call formula in
[`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html) must
represent the formula that will be used in
[`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html).
If you utilize the
[`tbl_survfit.data.frame()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
S3 method, this is handled for you.

## See also

Other tbl_survfit tools:
[`add_nevent.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_nevent.tbl_survfit.md)

## Examples

``` r
library(survival)

gts_survfit <-
  list(
    survfit(Surv(ttdeath, death) ~ grade, trial),
    survfit(Surv(ttdeath, death) ~ trt, trial)
  ) |>
  tbl_survfit(times = c(12, 24))

# Example 1 ----------------------------------
gts_survfit |>
  add_p()


  

Characteristic
```

**Time 12**

**Time 24**

**p-value**¹

Grade

  

  

0.072

    I

97% (93%, 100%)

51% (41%, 65%)

  

    II

82% (74%, 92%)

47% (37%, 61%)

  

    III

86% (78%, 95%)

33% (23%, 47%)

  

Chemotherapy Treatment

  

  

0.2

    Drug A

91% (85%, 97%)

47% (38%, 58%)

  

    Drug B

86% (80%, 93%)

41% (33%, 52%)

  

¹ Log-rank test

\# Example 2 ---------------------------------- \# Pass \`rho=\`
argument to \`survdiff()\` gts_survfit \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)(test
= "survdiff", test.args = [list](https://rdrr.io/r/base/list.html)(rho =
0.5))

[TABLE]
