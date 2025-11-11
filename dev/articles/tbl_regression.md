# tbl_regression() tutorial

## Introduction

The
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
function takes a **regression model object** in **R** and returns a
**formatted table of regression model results** that is
publication-ready. It is a simple way to summarize and present your
analysis results using **R**! Like
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html),
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
creates highly customizable analytic tables with sensible defaults.

This vignette will walk a reader through the
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
function, and the various functions available to modify and make
additions to an existing formatted regression table.

![animated](https://github.com/ddsjoberg/gtsummary/raw/main/data-raw/misc_files/tbl_mvregression_demo.gif)

*Behind the scenes:*
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
uses [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
to perform the initial model formatting, and can accommodate many
different model types (e.g. [`lm()`](https://rdrr.io/r/stats/lm.html),
[`glm()`](https://rdrr.io/r/stats/glm.html),
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html),
[`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html)
and others are [supported models](#supported-models) known to work with
{gtsummary}). It is also possible to specify your own function to tidy
the model results if needed.

## Setup

Before going through the tutorial, install and load {gtsummary}.

``` r
# install.packages("gtsummary")
library(gtsummary)
```

## Example data set

In this vignette we’ll be using the
[**`trial`**](https://www.danieldsjoberg.com/gtsummary/reference/trial.html)
data set which is included in the {gtsummary} package.

- This data set contains information from 200 patients who received one
  of two types of chemotherapy (Drug A or Drug B).

- The outcomes are tumor response and death.

- Each variable in the data frame has been assigned an **attribute
  label** (i.e. `attr(trial$trt, "label") == "Chemotherapy Treatment")`
  with the **[labelled](http://larmarange.github.io/labelled/)**
  package, which we highly recommend using. These labels are displayed
  in the {gtsummary} output table by default. Using {gtsummary} on a
  data frame without labels will simply print variable names, or there
  is an option to add labels later.

| Variable                                                           | Class     | Label                  |
|--------------------------------------------------------------------|-----------|------------------------|
| `trt`                                                              | character | Chemotherapy Treatment |
| `age`                                                              | numeric   | Age                    |
| `marker`                                                           | numeric   | Marker Level (ng/mL)   |
| `stage`                                                            | factor    | T Stage                |
| `grade`                                                            | factor    | Grade                  |
| `response`                                                         | integer   | Tumor Response         |
| `death`                                                            | integer   | Patient Died           |
| `ttdeath`                                                          | numeric   | Months to Death/Censor |
| Includes mix of continuous, dichotomous, and categorical variables |           |                        |

## Basic Usage

The default output from
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
is meant to be publication ready.

- Let’s start by creating a **logistic regression model** to **predict
  tumor response** using the variables age and grade from the
  [`trial`](https://www.danieldsjoberg.com/gtsummary/reference/trial.html)
  data set.

``` r
# build logistic regression model
m1 <- glm(response ~ age + stage, trial, family = binomial)

# view raw model results
summary(m1)$coefficients
#>                Estimate Std. Error    z value   Pr(>|z|)
#> (Intercept) -1.48622424 0.62022844 -2.3962530 0.01656365
#> age          0.01939109 0.01146813  1.6908683 0.09086195
#> stageT2     -0.54142643 0.44000267 -1.2305071 0.21850725
#> stageT3     -0.05953479 0.45042027 -0.1321761 0.89484501
#> stageT4     -0.23108633 0.44822835 -0.5155549 0.60616530
```

- We will then a **regression model table** to summarize and present
  these results in just one line of code from {gtsummary}.

``` r
tbl_regression(m1, exponentiate = TRUE)
```

[TABLE]

Note the sensible defaults with this basic usage (that can be customized
later):

- The model was recognized as logistic regression with coefficients
  exponentiated, so the **header displayed “OR”** for odds ratio.

- **Variable types** are automatically detected and **reference rows**
  are added for categorical variables.

- **Model estimates** and **confidence intervals** are **rounded** and
  formatted.

- Because the variables in the data set were
  [labelled](http://larmarange.github.io/labelled/), the **labels were
  carried through** into the {gtsummary} output table. Had the data not
  been labelled, the default is to display the variable name.

- Variable levels are **indented** and **footnotes** added.

## Customize Output

There are four primary ways to customize the output of the regression
model table.

1.  Modify
    [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
    function input arguments
2.  Add additional data/information to a summary table with `add_*()`
    functions
3.  Modify summary table appearance with the {gtsummary} functions
4.  Modify table appearance with {gt} package functions

### Modifying function arguments

The
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
function includes many arguments for modifying the appearance.

| Argument          | Description                                                                                                                                                                                       |
|-------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `label`           | modify variable labels in table                                                                                                                                                                   |
| `exponentiate`    | exponentiate model coefficients                                                                                                                                                                   |
| `include`         | names of variables to include in output. Default is all variables                                                                                                                                 |
| `show_single_row` | By default, categorical variables are printed on multiple rows. If a variable is dichotomous and you wish to print the regression coefficient on a single row, include the variable name(s) here. |
| `conf.level`      | confidence level of confidence interval                                                                                                                                                           |
| `intercept`       | indicates whether to include the intercept                                                                                                                                                        |
| `estimate_fun`    | function to round and format coefficient estimates                                                                                                                                                |
| `pvalue_fun`      | function to round and format p-values                                                                                                                                                             |
| `tidy_fun`        | function to specify/customize tidier function                                                                                                                                                     |

### {gtsummary} functions to add information

The {gtsummary} package has built-in functions for adding to results
from
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md).
The following functions add columns and/or information to the regression
table.

| Function                                                                                           | Description                                                  |
|----------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| [`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)         | adds the global p-value for a categorical variables          |
| [`add_glance_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_glance.md) | adds statistics from \`broom::glance()\` as source note      |
| [`add_vif()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_vif.md)                   | adds column of the variance inflation factors (VIF)          |
| [`add_q()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_q.md)                       | add a column of q values to control for multiple comparisons |

### {gtsummary} functions to format table

The {gtsummary} package comes with functions specifically made to modify
and format summary tables.

| Function                                                                                                       | Description                   |
|----------------------------------------------------------------------------------------------------------------|-------------------------------|
| [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)                          | update column headers         |
| [`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)       | update column header footnote |
| [`modify_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)         | update table body footnote    |
| [`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)                 | update spanning headers       |
| [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)                 | update table caption/title    |
| [`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)      | bold variable labels          |
| [`bold_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)      | bold variable levels          |
| [`italicize_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md) | italicize variable labels     |
| [`italicize_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md) | italicize variable levels     |
| [`bold_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_p.md)                                 | bold significant p-values     |

### {gt} functions to format table

The [{gt} package](https://gt.rstudio.com/index.html) is packed with
many great functions for modifying table output—too many to list here.
Review the package’s website for a full listing.

To use the {gt} package functions with {gtsummary} tables, the
regression table must first be converted into a {gt} object. To this
end, use the
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
function after modifications have been completed with {gtsummary}
functions.

``` r
m1 |>
  tbl_regression(exponentiate = TRUE) |>
  as_gt() |>
  gt::tab_source_note(gt::md("*This data is simulated*"))
```

[TABLE]

### Example

There are formatting options available, such as adding bold and italics
to text. In the example below,  
- Coefficients are **exponentiated** to give odds ratios  
- **Global p-values** for Stage are reported - Large p-values are
rounded to **two decimal places**  
- P-values less than 0.10 are **bold** - Variable labels are **bold**  
- Variable levels are **italicized**

``` r
# format results into data frame with global p-values
m1 |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
  ) |>
  add_global_p() |>
  bold_p(t = 0.10) |>
  bold_labels() |>
  italicize_levels()
```

[TABLE]

## Univariate Regression

The
[`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
function produces a table of univariate regression models. The function
is a wrapper for
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md),
and as a result, accepts nearly identical function arguments. The
function’s results can be modified in similar ways to
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md).

``` r
trial |>
  tbl_uvregression(
    method = glm,
    y = response,
    include = c(age, grade),
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2)
  ) |>
  add_global_p() |> # add global p-value
  add_nevent() |> # add number of events of the outcome
  add_q() |> # adjusts global p-values for multiple testing
  bold_p() |> # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.10, q = TRUE) |> # now bold q-values under the threshold of 0.10
  bold_labels()
```

[TABLE]

## Setting Default Options

The {gtsummary} regression functions and their related functions have
sensible defaults for rounding and formatting results. If you, however,
would like to change the defaults there are a few options. The default
options can be changed using the {gtsummary} themes function
[`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md).
The package includes pre-specified themes, and you can also create your
own. Themes can control baseline behavior, for example, how p-values are
rounded, coefficients are rounded, default headers, confidence levels,
etc. For details on creating a theme and setting personal defaults,
visit the [themes
vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html).

## Supported Models

Below is a listing of known and tested models supported by
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md).
If a model follows a standard format and has a tidier, it’s likely to be
supported as well, even if not listed below.

| Model                                                                           | Details                                                                                                                                                                                                                                                                     |
|---------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `betareg::betareg()`                                                            | Use `tidy_parameters()` as `tidy_fun` with `component` argument to control with coefficients to return. [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) does not support the `exponentiate` argument for betareg models, use `tidy_parameters()` instead. |
| `biglm::bigglm()`                                                               |                                                                                                                                                                                                                                                                             |
| `brms::brm()`                                                                   | `broom.mixed` package required                                                                                                                                                                                                                                              |
| [`cmprsk::crr()`](https://rdrr.io/pkg/cmprsk/man/crr.html)                      | Limited support. It is recommended to use `tidycmprsk::crr()` instead.                                                                                                                                                                                                      |
| `fixest::feglm()`                                                               | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `fixest::femlm()`                                                               | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `fixest::feNmlm()`                                                              | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `fixest::feols()`                                                               | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `gam::gam()`                                                                    |                                                                                                                                                                                                                                                                             |
| [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html)              |                                                                                                                                                                                                                                                                             |
| `glmmTMB::glmmTMB()`                                                            | `broom.mixed` package required                                                                                                                                                                                                                                              |
| `glmtoolbox::glmgee()`                                                          |                                                                                                                                                                                                                                                                             |
| `lavaan::lavaan()`                                                              | Limited support for categorical variables                                                                                                                                                                                                                                   |
| `lfe::felm()`                                                                   |                                                                                                                                                                                                                                                                             |
| [`lme4::glmer.nb()`](https://rdrr.io/pkg/lme4/man/glmer.nb.html)                | `broom.mixed` package required                                                                                                                                                                                                                                              |
| [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html)                      | `broom.mixed` package required                                                                                                                                                                                                                                              |
| [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)                        | `broom.mixed` package required                                                                                                                                                                                                                                              |
| `logitr::logitr()`                                                              | Requires logitr \>= 0.8.0                                                                                                                                                                                                                                                   |
| [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html)                    |                                                                                                                                                                                                                                                                             |
| [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html)                        |                                                                                                                                                                                                                                                                             |
| [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html)                          | Use default tidier [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) for smooth terms only, or [`gtsummary::tidy_gam()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md) to include parametric terms                              |
| [`mice::mira`](https://amices.org/mice/reference/mira.html)                     | Limited support. If `mod` is a `mira` object, use `tidy_fun = function(x, ...) {mice::pool(x) |> mice::tidy(...)}`                                                                                                                                                          |
| `mmrm::mmrm()`                                                                  |                                                                                                                                                                                                                                                                             |
| `multgee::nomLORgee()`                                                          | Use `tidy_multgee()` as `tidy_fun`.                                                                                                                                                                                                                                         |
| `multgee::ordLORgee()`                                                          | Use `tidy_multgee()` as `tidy_fun`.                                                                                                                                                                                                                                         |
| [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)                |                                                                                                                                                                                                                                                                             |
| [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html)                    | Limited support for models with nominal predictors.                                                                                                                                                                                                                         |
| [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html)                  | Limited support for models with nominal predictors.                                                                                                                                                                                                                         |
| [`parsnip::model_fit`](https://parsnip.tidymodels.org/reference/model_fit.html) | Supported as long as the type of model and the engine is supported.                                                                                                                                                                                                         |
| `plm::plm()`                                                                    |                                                                                                                                                                                                                                                                             |
| `pscl::hurdle()`                                                                | Use `tidy_zeroinfl()` as `tidy_fun`.                                                                                                                                                                                                                                        |
| `pscl::zeroinfl()`                                                              | Use `tidy_zeroinfl()` as `tidy_fun`.                                                                                                                                                                                                                                        |
| [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html)                    | If several quantiles are estimated, use `tidy_with_broom_or_parameters()` tidier, the default tidier used by `tidy_plus_plus()`.                                                                                                                                            |
| `rstanarm::stan_glm()`                                                          | `broom.mixed` package required                                                                                                                                                                                                                                              |
| [`stats::aov()`](https://rdrr.io/r/stats/aov.html)                              | Reference rows are not relevant for such models.                                                                                                                                                                                                                            |
| [`stats::glm()`](https://rdrr.io/r/stats/glm.html)                              |                                                                                                                                                                                                                                                                             |
| [`stats::lm()`](https://rdrr.io/r/stats/lm.html)                                |                                                                                                                                                                                                                                                                             |
| [`stats::nls()`](https://rdrr.io/r/stats/nls.html)                              | Limited support                                                                                                                                                                                                                                                             |
| [`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html)            |                                                                                                                                                                                                                                                                             |
| [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)                |                                                                                                                                                                                                                                                                             |
| [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html)                |                                                                                                                                                                                                                                                                             |
| [`survival::cch()`](https://rdrr.io/pkg/survival/man/cch.html)                  | Experimental support.                                                                                                                                                                                                                                                       |
| [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)            |                                                                                                                                                                                                                                                                             |
| [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)              |                                                                                                                                                                                                                                                                             |
| [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html)          |                                                                                                                                                                                                                                                                             |
| `svyVGAM::svy_vglm()`                                                           | Experimental support. It is recommended to use `tidy_svy_vglm()` as `tidy_fun`.                                                                                                                                                                                             |
| `tidycmprsk::crr()`                                                             |                                                                                                                                                                                                                                                                             |
| `VGAM::vgam()`                                                                  | Experimental support. It is recommended to use `tidy_vgam()` as `tidy_fun`.                                                                                                                                                                                                 |
| `VGAM::vglm()`                                                                  | Experimental support. It is recommended to use `tidy_vgam()` as `tidy_fun`.                                                                                                                                                                                                 |
