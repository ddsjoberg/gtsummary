# Custom tidiers

Collection of tidiers that can be utilized in gtsummary. See details
below.

## Usage

``` r
tidy_standardize(
  x,
  exponentiate = FALSE,
  conf.level = 0.95,
  conf.int = TRUE,
  ...,
  quiet = FALSE
)

tidy_bootstrap(
  x,
  exponentiate = FALSE,
  conf.level = 0.95,
  conf.int = TRUE,
  ...,
  quiet = FALSE
)

tidy_robust(
  x,
  exponentiate = FALSE,
  conf.level = 0.95,
  conf.int = TRUE,
  vcov = NULL,
  vcov_args = NULL,
  ...,
  quiet = FALSE
)

pool_and_tidy_mice(x, pool.args = NULL, ..., quiet = FALSE)

tidy_gam(x, conf.int = FALSE, exponentiate = FALSE, conf.level = 0.95, ...)

tidy_wald_test(x, tidy_fun = NULL, vcov = stats::vcov(x), ...)
```

## Arguments

- x:

  (`model`)  
  Regression model object

- exponentiate:

  (scalar `logical`)  
  Logical indicating whether to exponentiate the coefficient estimates.
  Default is `FALSE`.

- conf.level:

  (scalar `real`)  
  Confidence level for confidence interval/credible interval. Defaults
  to `0.95`.

- conf.int:

  (scalar `logical`)  
  Logical indicating whether or not to include a confidence interval in
  the output. Default is `TRUE`.

- ...:

  Arguments passed to method;

  - `pool_and_tidy_mice()`: `mice::tidy(x, ...)`

  - `tidy_standardize()`: `parameters::standardize_parameters(x, ...)`

  - `tidy_bootstrap()`: `parameters::bootstrap_parameters(x, ...)`

  - `tidy_robust()`: `parameters::model_parameters(x, ...)`

- quiet:

  **\[deprecated\]**

- vcov, vcov_args:

  - `tidy_robust()`: Arguments passed to
    [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).
    At least one of these arguments **must** be specified.

  - `tidy_wald_test()`: `vcov` is the covariance matrix of the model
    with default [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html).

- pool.args:

  (named `list`)  
  Named list of arguments passed to
  [`mice::pool()`](https://amices.org/mice/reference/pool.html) in
  `pool_and_tidy_mice()`. Default is `NULL`

- tidy_fun:

  (`function`)  
  Tidier function for the model. Default is to use
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html). If
  an error occurs, the tidying of the model is attempted with
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  if installed.

## Regression Model Tidiers

These tidiers are passed to
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
and
[`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
to obtain modified results.

- `tidy_standardize()` tidier to report standardized coefficients. The
  [parameters](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  package includes a wonderful function to estimate standardized
  coefficients. The tidier uses the output from
  [`parameters::standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html),
  and merely takes the result and puts it in
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  format.

- `tidy_bootstrap()` tidier to report bootstrapped coefficients. The
  [parameters](https://easystats.github.io/parameters/reference/model_parameters.default.html)
  package includes a wonderful function to estimate bootstrapped
  coefficients. The tidier uses the output from
  `parameters::bootstrap_parameters(test = "p")`, and merely takes the
  result and puts it in
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  format.

- `tidy_robust()` tidier to report robust standard errors, confidence
  intervals, and p-values. The
  [parameters](https://easystats.github.io/parameters/reference/model_parameters.default.html)
  package includes a wonderful function to calculate robust standard
  errors, confidence intervals, and p-values The tidier uses the output
  from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  and merely takes the result and puts it in
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  format. To use this function with
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md),
  pass a function with the arguments for `tidy_robust()` populated.

- `pool_and_tidy_mice()` tidier to report models resulting from multiply
  imputed data using the mice package. Pass the mice model object
  *before* the model results have been pooled. See example.

## Other Tidiers

- `tidy_wald_test()` tidier to report Wald p-values, wrapping the
  [`aod::wald.test()`](https://rdrr.io/pkg/aod/man/wald.test.html)
  function. Use this tidier with
  `add_global_p(anova_fun = tidy_wald_test)`

## Examples

``` r
# Example 1 ----------------------------------
mod <- lm(age ~ marker + grade, trial)

tbl_stnd <- tbl_regression(mod, tidy_fun = tidy_standardize)
tbl <- tbl_regression(mod)

tidy_standardize_ex1 <-
  tbl_merge(
    list(tbl_stnd, tbl),
    tab_spanner = c("**Standardized Model**", "**Original Model**")
  )

# Example 2 ----------------------------------
# use "posthoc" method for coef calculation
tbl_regression(mod, tidy_fun = \(x, ...) tidy_standardize(x, method = "posthoc", ...))


  

Characteristic
```

**Beta**

**95% CI**

Marker Level (ng/mL)

0.00

-0.15, 0.15

Grade

  

  

    I

—

—

    II

0.04

-0.32, 0.41

    III

0.17

-0.20, 0.53

Abbreviation: CI = Confidence Interval

\# Example 3 ---------------------------------- \# Multiple Imputation
using the mice package
[set.seed](https://rdrr.io/r/base/Random.html)(1123)
pool_and_tidy_mice_ex3 \<-
[suppressWarnings](https://rdrr.io/r/base/warning.html)(mice::[mice](https://amices.org/mice/reference/mice.html)(trial,
m = 2)) \|\>
[with](https://rdrr.io/r/base/with.html)([lm](https://rdrr.io/r/stats/lm.html)(age
~ marker + grade)) \|\>
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)()
\#\> \#\> iter imp variable \#\> 1 1 age marker response \#\> 1 2 age
marker response \#\> 2 1 age marker response \#\> 2 2 age marker
response \#\> 3 1 age marker response \#\> 3 2 age marker response \#\>
4 1 age marker response \#\> 4 2 age marker response \#\> 5 1 age marker
response \#\> 5 2 age marker response
