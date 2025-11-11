# Add the global p-values

This function uses
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) (by default) to
calculate global p-values for model covariates. Output from
`tbl_regression` and `tbl_uvregression` objects supported.

## Usage

``` r
add_global_p(x, ...)

# S3 method for class 'tbl_regression'
add_global_p(
  x,
  include = everything(),
  keep = FALSE,
  anova_fun = global_pvalue_fun,
  type = "III",
  quiet,
  ...
)

# S3 method for class 'tbl_uvregression'
add_global_p(
  x,
  include = everything(),
  keep = FALSE,
  anova_fun = global_pvalue_fun,
  type = "III",
  quiet,
  ...
)
```

## Arguments

- x:

  (`tbl_regression`, `tbl_uvregression`)  
  Object with class `'tbl_regression'` or `'tbl_uvregression'`

- ...:

  Additional arguments to be passed to
  [`car::Anova`](https://rdrr.io/pkg/car/man/Anova.html),
  [`aod::wald.test()`](https://rdrr.io/pkg/aod/man/wald.test.html) or
  `anova_fun` (if specified)

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to calculate global p-value for. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)

- keep:

  (scalar `logical`)  
  Logical argument indicating whether to also retain the individual
  p-values in the table output for each level of the categorical
  variable. Default is `FALSE`.

- anova_fun:

  (`function`)  
  Function used to calculate global p-values. Default is generic
  [`global_pvalue_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/global_pvalue_fun.md),
  which wraps [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)
  for most models. The `type` argument is passed to this function. See
  help file for details.

  To pass a custom function, it must accept as its first argument is a
  model. Note that anything passed in `...` will be passed to this
  function. The function must return an object of class `'cards'` (see
  [`cardx::ard_car_anova()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_car_anova.html)
  as an example), or a tibble with columns `'term'` and `'p.value'`
  (e.g. `\(x, type, ...) car::Anova(x, type, ...) |> broom::tidy()`).

- type:

  Type argument passed to `anova_fun`. Default is `"III"`

- quiet:

  **\[deprecated\]**

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
lm(marker ~ age + grade, trial) |>
  tbl_regression() |>
  add_global_p()


  

Characteristic
```

**Beta**

**95% CI**

**p-value**

Age

0.00

-0.01, 0.01

\>0.9

Grade

  

  

0.047

    I

—

—

  

    II

-0.38

-0.69, -0.07

  

    III

-0.12

-0.43, 0.19

  

Abbreviation: CI = Confidence Interval

\# Example 2 ----------------------------------
trial\[[c](https://rdrr.io/r/base/c.html)("response", "age", "trt",
"grade")\] \|\>
[tbl_uvregression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)(
method = glm, y = response, method.args =
[list](https://rdrr.io/r/base/list.html)(family = binomial),
exponentiate = TRUE ) \|\> add_global_p()

[TABLE]
