# Regression model summary

This function takes a regression model object and returns a formatted
table that is publication-ready. The function is customizable allowing
the user to create bespoke regression model summary tables. Review the
[`tbl_regression()`
vignette](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
for detailed examples.

## Usage

``` r
tbl_regression(x, ...)

# Default S3 method
tbl_regression(
  x,
  label = NULL,
  exponentiate = FALSE,
  include = everything(),
  show_single_row = NULL,
  conf.level = 0.95,
  intercept = FALSE,
  estimate_fun = ifelse(exponentiate, label_style_ratio(), label_style_sigfig()),
  pvalue_fun = label_style_pvalue(digits = 1),
  tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
  add_estimate_to_reference_rows = FALSE,
  conf.int = TRUE,
  ...
)
```

## Arguments

- x:

  (regression model)  
  Regression model object

- ...:

  Additional arguments passed to
  [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html).

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to change variables labels, e.g.
  `list(age = "Age", stage = "Path T Stage")`

- exponentiate:

  (scalar `logical`)  
  Logical indicating whether to exponentiate the coefficient estimates.
  Default is `FALSE`.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in output. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- show_single_row:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  By default categorical variables are printed on multiple rows. If a
  variable is dichotomous (e.g. Yes/No) and you wish to print the
  regression coefficient on a single row, include the variable name(s)
  here.

- conf.level:

  (scalar `real`)  
  Confidence level for confidence interval/credible interval. Defaults
  to `0.95`.

- intercept:

  (scalar `logical`)  
  Indicates whether to include the intercept in the output. Default is
  `FALSE`

- estimate_fun:

  (`function`)  
  Function to round and format coefficient estimates. Default is
  [`label_style_sigfig()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)
  when the coefficients are not transformed, and
  [`label_style_ratio()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)
  when the coefficients have been exponentiated.

- pvalue_fun:

  (`function`)  
  Function to round and format p-values. Default is
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md).

- tidy_fun:

  (`function`)  
  Tidier function for the model. Default is to use
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html). If
  an error occurs, the tidying of the model is attempted with
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  if installed.

- add_estimate_to_reference_rows:

  (scalar `logical`)  
  Add a reference value. Default is `FALSE`.

- conf.int:

  (scalar `logical`)  
  Logical indicating whether or not to include a confidence interval in
  the output. Default is `TRUE`.

## Value

A `tbl_regression` object

## Methods

The default method for `tbl_regression()` model summary uses
`broom::tidy(x)` to perform the initial tidying of the model object.
There are, however, a few models that use
[modifications](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression_methods.md).

- `"parsnip/workflows"`: If the model was prepared using
  parsnip/workflows, the original model fit is extracted and the
  original `x=` argument is replaced with the model fit. This will
  typically go unnoticed; however,if you've provided a custom tidier in
  `tidy_fun=` the tidier will be applied to the model fit object and not
  the parsnip/workflows object.

- `"survreg"`: The scale parameter is removed,
  `broom::tidy(x) %>% dplyr::filter(term != "Log(scale)")`

- `"multinom"`: This multinomial outcome is complex, with one line per
  covariate per outcome (less the reference group)

- `"gam"`: Uses the internal tidier
  [`tidy_gam()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md)
  to print both parametric and smooth terms.

- `"lmerMod"`, `"glmerMod"`, `"glmmTMB"`, `"glmmadmb"`, `"stanreg"`,
  `"brmsfit"`: These mixed effects models use
  `broom.mixed::tidy(x, effects = "fixed")`. Specify
  `tidy_fun = broom.mixed::tidy` to print the random components.

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
glm(response ~ age + grade, trial, family = binomial()) |>
  tbl_regression(exponentiate = TRUE)


  

Characteristic
```

**OR**

**95% CI**

**p-value**

Age

1.02

1.00, 1.04

0.10

Grade

  

  

  

    I

—

—

  

    II

0.85

0.39, 1.85

0.7

    III

1.01

0.47, 2.16

\>0.9

Abbreviations: CI = Confidence Interval, OR = Odds Ratio
