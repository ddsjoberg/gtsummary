# Univariable regression model summary

This function estimates univariable regression models and returns them
in a publication-ready table. It can create regression models holding
either a covariate or an outcome constant.

## Usage

``` r
tbl_uvregression(data, ...)

# S3 method for class 'data.frame'
tbl_uvregression(
  data,
  y = NULL,
  x = NULL,
  method,
  method.args = list(),
  exponentiate = FALSE,
  label = NULL,
  include = everything(),
  tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
  hide_n = FALSE,
  show_single_row = NULL,
  conf.level = 0.95,
  estimate_fun = ifelse(exponentiate, label_style_ratio(), label_style_sigfig()),
  pvalue_fun = label_style_pvalue(digits = 1),
  formula = "{y} ~ {x}",
  add_estimate_to_reference_rows = FALSE,
  conf.int = TRUE,
  ...
)

# S3 method for class 'survey.design'
tbl_uvregression(
  data,
  y = NULL,
  x = NULL,
  method,
  method.args = list(),
  exponentiate = FALSE,
  label = NULL,
  include = everything(),
  tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
  hide_n = FALSE,
  show_single_row = NULL,
  conf.level = 0.95,
  estimate_fun = ifelse(exponentiate, label_style_ratio(), label_style_sigfig()),
  pvalue_fun = label_style_pvalue(digits = 1),
  formula = "{y} ~ {x}",
  add_estimate_to_reference_rows = FALSE,
  conf.int = TRUE,
  ...
)
```

## Arguments

- data:

  (`data.frame`, `survey.design`)  
  A data frame or a survey design object.

- ...:

  Additional arguments passed to
  [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html).

- y, x:

  (`expression`, `string`)  
  Model outcome (e.g. `y=recurrence` or `y=Surv(time, recur)`) or
  covariate (e.g. `x=trt`. All other column specified in `include` will
  be regressed against the constant `y` or `x`. Specify one and only one
  of `y` or `x`.

- method:

  (`string`/`function`)  
  Regression method or function, e.g.
  [lm](https://rdrr.io/r/stats/lm.html),
  [glm](https://rdrr.io/r/stats/glm.html),
  [survival::coxph](https://rdrr.io/pkg/survival/man/coxph.html),
  [`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html), etc.
  Methods may be passed as functions (`method=lm`) or as strings
  (`method='lm'`).

- method.args:

  (named `list`)  
  Named list of arguments passed to `method`.

- exponentiate:

  (scalar `logical`)  
  Logical indicating whether to exponentiate the coefficient estimates.
  Default is `FALSE`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to change variables labels, e.g.
  `list(age = "Age", stage = "Path T Stage")`

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in output. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- tidy_fun:

  (`function`)  
  Tidier function for the model. Default is to use
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html). If
  an error occurs, the tidying of the model is attempted with
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  if installed.

- hide_n:

  (scalar `logical`)  
  Hide N column. Default is `FALSE`

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

- formula:

  (`string`)  
  String of the model formula. Uses
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  syntax. Default is `"{y} ~ {x}"`, where `{y}` is the dependent
  variable, and `{x}` represents a single covariate. For a random
  intercept model, the formula may be
  `formula = "{y} ~ {x} + (1 | gear)"`.

- add_estimate_to_reference_rows:

  (scalar `logical`)  
  Add a reference value. Default is `FALSE`.

- conf.int:

  (scalar `logical`)  
  Logical indicating whether or not to include a confidence interval in
  the output. Default is `TRUE`.

## Value

A `tbl_uvregression` object

## `x` and `y` arguments

For models holding outcome constant, the function takes as arguments a
data frame, the type of regression model, and the outcome variable `y=`.
Each column in the data frame is regressed on the specified outcome. The
`tbl_uvregression()` function arguments are similar to the
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
arguments. Review the [tbl_uvregression
vignette](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression)
for detailed examples.

You may alternatively hold a single covariate constant. For this, pass a
data frame, the type of regression model, and a single covariate in the
`x=` argument. Each column of the data frame will serve as the outcome
in a univariate regression model. Take care using the `x` argument that
each of the columns in the data frame are appropriate for the same type
of model, e.g. they are all continuous variables appropriate for
[lm](https://rdrr.io/r/stats/lm.html), or dichotomous variables
appropriate for logistic regression with
[glm](https://rdrr.io/r/stats/glm.html).

## Methods

The default method for
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
model summary uses `broom::tidy(x)` to perform the initial tidying of
the model object. There are, however, a few models that use
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

## See also

See tbl_regression
[vignette](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression)
for detailed examples

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
tbl_uvregression(
  trial,
  method = glm,
  y = response,
  method.args = list(family = binomial),
  exponentiate = TRUE,
  include = c("age", "grade")
)


  

Characteristic
```

**N**

**OR**

**95% CI**

**p-value**

Age

183

1.02

1.00, 1.04

0.10

Grade

193

  

  

  

    I

  

—

—

  

    II

  

0.95

0.45, 2.00

0.9

    III

  

1.10

0.52, 2.29

0.8

Abbreviations: CI = Confidence Interval, OR = Odds Ratio

\# Example 2 ---------------------------------- \# rounding pvalues to 2
decimal places
[library](https://rdrr.io/r/base/library.html)([survival](https://github.com/therneau/survival))
tbl_uvregression( trial, method = coxph, y =
[Surv](https://rdrr.io/pkg/survival/man/Surv.html)(ttdeath, death),
exponentiate = TRUE, include = [c](https://rdrr.io/r/base/c.html)("age",
"grade", "response"), pvalue_fun =
[label_style_pvalue](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)(digits
= 2) )

[TABLE]
