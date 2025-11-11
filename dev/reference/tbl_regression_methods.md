# Methods for tbl_regression

Most regression models are handled by
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md),
which uses
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
to perform initial tidying of results. There are, however, some model
types that have modified default printing behavior. Those methods are
listed below.

## Usage

``` r
# S3 method for class 'model_fit'
tbl_regression(x, ...)

# S3 method for class 'workflow'
tbl_regression(x, ...)

# S3 method for class 'survreg'
tbl_regression(
  x,
  tidy_fun = function(x, ...) dplyr::filter(broom::tidy(x, ...), .data$term !=
    "Log(scale)"),
  ...
)

# S3 method for class 'mira'
tbl_regression(x, tidy_fun = pool_and_tidy_mice, ...)

# S3 method for class 'mipo'
tbl_regression(x, ...)

# S3 method for class 'lmerMod'
tbl_regression(
  x,
  tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"),
  ...
)

# S3 method for class 'glmerMod'
tbl_regression(
  x,
  tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"),
  ...
)

# S3 method for class 'glmmTMB'
tbl_regression(
  x,
  tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"),
  ...
)

# S3 method for class 'glmmadmb'
tbl_regression(
  x,
  tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"),
  ...
)

# S3 method for class 'stanreg'
tbl_regression(
  x,
  tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"),
  ...
)

# S3 method for class 'brmsfit'
tbl_regression(
  x,
  tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"),
  ...
)

# S3 method for class 'gam'
tbl_regression(x, tidy_fun = tidy_gam, ...)

# S3 method for class 'crr'
tbl_regression(x, ...)
```

## Arguments

- x:

  (regression model)  
  Regression model object

- ...:

  arguments passed to
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)

- tidy_fun:

  (`function`)  
  Tidier function for the model. Default is to use
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html). If
  an error occurs, the tidying of the model is attempted with
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
  if installed.

## Methods

The default method for
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
model summary uses `broom::tidy(x)` to perform the initial tidying of
the model object. There are, however, a few models that use
modifications.

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
