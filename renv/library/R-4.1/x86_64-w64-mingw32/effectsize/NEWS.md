# effectsize 0.4.5

## New features

- `eta_squared()` family now indicate the type of sum-of-squares used.
- `rank_biserial()` estimates CIs using the normal approximation (previously used bootstrapping).  
- `hedges_g()` now used exact bias correction (thanks to @mdelacre for the suggestion!)  
- `glass_delta()` now estimates CIs using the NCP method based on Algina et al (2006).

## Bug fixes

- `eta_squared()` family returns correctly returns the type 2/3 effect sizes for mixed ANOVAs fit with `afex`.
- `cohens_d()` family now correctly deals with missing factor levels ( #318 )
- `cohens_d()` / `hedges_g()` minor fix for CI with unequal variances.  


## Changes

- `mad_pooled()` (the robust version of `sd_pooled()`) now correctly pools the the two samples.

# effectsize 0.4.4-1

## New features

- `standardize_parameters()` + `eta_sqaured()` support `tidymodels` (when that the underlying model is supported; #311 ).
- `cohens_d()` family now supports `Pairs()` objects as input.
- `standardize_parameters()` gains the `include_response` argument (default to `TRUE`) ( #309 ).

## Bug fixes

- `kendalls_w()` now actually returns correct effect size. Previous estimates were incorrect, and based on transposing the groups and blocks.

# effectsize 0.4.4

`effectsize` now supports `R >= 3.4`.

## New features

- `standardize_parameters()` now supports bootstrapped estimates (from `parameters::bootstrap_model()` and `parameters::bootstrap_parameters()`).
- `unstandardize()` which will reverse the effects of `standardize()`.
- `interpret_kendalls_w()` to interpret Kendall's coefficient of concordance.
- `eta_squared()` family of functions can now also return effect sizes for the intercept by setting `include_intercept = TRUE` ( #156 ).

## Bug fixes

- `standardize()` can now deal with dates ( #300 ).

# effectsize 0.4.3

## Breaking Changes

- `oddsratio()` and `riskratio()` - order of groups has been changed (the
  *first* groups is now the **treatment group**, and the *second* group is the
  **control group**), so that effect sizes are given as *treatment over control*
  (treatment / control) (previously was reversed). This is done to be consistent
  with other functions in R and in `effectsize`.

## New features

- `cohens_h()` effect size for comparing two independent proportions.

- `rank_biserial()`, `cliffs_delta()`, `rank_epsilon_squared()` and
  `kendalls_w()` functions for effect sizes for rank-based tests.

- `adjust()` gains `keep_intercept` argument to keep the intercept.

- `eta_squared()` family of functions supports `Anova.mlm` objects (from the
  `car` package).

- `effectsize()`:

  - supports Cohen's *g* for McNemar's test.

  - Extracts OR from Fisher's Exact Test in the 2x2 case.

- `eta2_to_f2()` / `f2_to_eta2()` to convert between two types of effect sizes
  for ANOVA ( #240 ).

- `cohens_d()` family of functions gain `mu` argument.

## Bug fixes

- `adjust()` properly works when `multilevel = TRUE`.

- `cohens_d()` family / `sd_pooled()` now properly fails when given a missing
  column name.

## Changes

- `effectsize()` for `htest` objects now tries first to extract the data used
  for testing, and computed the effect size directly on that data.

- `cohens_d()` family / `sd_pooled()` now respect any transformations (e.g.
  `I(log(x) - 3) ~ factor(y)`) in a passed formula.

- `eta_squared()` family of functions gains a `verbose` argument.

- `verbose` argument more strictly respected.

- `glass_delta()` returns CIs based on the bootstrap.

# effectsize 0.4.1

## Breaking Changes

- `cohens_d()` and `glass_delta()`: The `correction` argument has been
  deprecated, in favor of it being correctly implemented in `hedges_g()` ( #222
  ).

- `eta_squared_posterior()` no longer uses `car::Anova()` by default.

## New features

- `effectsize()` gains `type = ` argument for specifying which effect size to
  return.

- `eta_squared_posterior()` can return a generalized Eta squared.

- `oddsratio()` and `riskratio()` functions for 2-by-2 contingency tables.

- `standardize()` gains support for `mediation::mediate()` models.

- `eta_squared()` family available for `manova` objects.

## Changes

- `eta_squared()` family of functions returns non-partial effect size for
  one-way between subjects design (#180).

## Bug fixes

- `hedges_g()` correctly implements the available bias correction methods ( #222
  ).

- Fixed width of CI for Cohen's *d* and Hedges' *g* when using *non*-pooled SD.

# effectsize 0.4.0

## Breaking Changes

- `standardize_parameters()` for multi-component models (such as zero-inflated)
  now returns the unstandardized parameters in some cases where standardization
  is not possible (previously returned `NA`s).

- Column name changes:

  - `eta_squared()` / `F_to_eta2` families of function now has the `Eta2`
    format, where previously was `Eta_Sq`.

  - `cramers_v` is now `Cramers_v`

## New features

- `effectsize()` added support for `BayesFactor` objects (Cohen's *d*, Cramer's
  *v*, and *r*).

- `cohens_g()` effect size for paired contingency tables.

- Generalized Eta Squared now available via `eta_squared(generalized = ...)`.

- `eta_squared()`, `omega_squared()` and `epsilon_squared()` fully support
  `aovlist`, `afex_aov` and `mlm` (or `maov`) objects.

- `standardize_parameters()` can now return Odds ratios / IRRs (or any
  exponentiated parameter) by setting `exponentiate = TRUE`.

- Added `cohens_f_squared()` and `F_to_f2()` for Cohen's *f*-squared.

- `cohens_f()` / `cohens_f_squared()`can be used to estimate Cohen's *f* for the
  R-squared change between two models.

- `standardize()` and `standardize_info()` work with weighted models / data (
  #82 ).

- Added `hardlyworking` (simulated) dataset, for use in examples.

- `interpret_*` ( #131 ):

  - `interpret_omega_squared()` added `"cohen1992"` rule.

  - `interpret_p()` added *Redefine statistical significance* rules.

- `oddsratio_to_riskratio()` for converting OR to RR.

## Changes

- CIs for Omega-/Epsilon-squared and Adjusted Phi/Cramer's V return 0s instead
  of negative values.

- `standardize()` for data frames gains the `remove_na` argument for dealing
  with `NA`s ( #147 ).

- `standardize()` and `standardize_info()` now (and by extension,
  `standardize_parameters()`) respect the weights in weighted models when
  standardizing ( #82 ).

- Internal changes to `standardize_parameters()` (reducing co-dependency with
  `parameters`) - argument `parameters` has been dropped.

## Bug fixes

- `ranktransform(sign = TURE)` correctly (doesn't) deal with zeros.

- `effectsize()` for `htest` works with Spearman and Kendall correlations ( #165
  ).

- `cramers_v()` and `phi()` now work with goodness-of-fit data ( #158 )

- `standardize_parameters()` for post-hoc correctly standardizes transformed
  outcome.

- Setting `two_sd = TRUE` in `standardize()` and `standardize_parameters()`
  (correctly) on uses 2-SDs of the predictors (and not the response).

- `standardize_info()` / `standardize_parameters(method = "posthoc")` work for
  zero-inflated models ( #135 )

- `standardize_info(include_pseudo = TRUE)` / `standardize_parameters(method =
  "pseudo")` are less sensitive in detecting between-group variation of
  within-group variables.

- `interpret_oddsratio()` correctly treats extremely small odds the same as
  treats extremely large ones.

# effectsize 0.3.3

## New features

- `standardize_parameters(method = "pseudo")` returns pseudo-standardized
  coefficients for (G)LMM models.

- `d_to_common_language()` for common language measures of standardized
  differences (a-la Cohen's d).

## Changes

- `r_to_odds()` family is now deprecated in favor of `r_to_oddsratio()`.

- `interpret_odds()` is now deprecated in favor of `interpret_oddsratio()`

## Bug fixes

- `phi()` and `cramers_v()` did not respect the CI argument ( #111 ).

- `standardize()` / `standardize_parameters()` properly deal with transformed
  data in the model formula ( #113 ).

- `odds_to_probs()` was mis-treating impossible odds (NEVER TELL ME THE ODDS!
  #123 )

# effectsize 0.3.2

## New features

- `eta_squared_posterior()` for estimating Eta Squared for Bayesian models.

- `eta_squared()`, `omega_squared()` and `epsilon_squared()` now works with

  - `ols` / `rms` models.

- `effectsize()` for class `htest` supports `oneway.test(...)`.

## Bug fixes

- Fix minor miss-calculation of Chi-squared for 2*2 table with small samples (
  #102 ).

- Fixed miss-calculation of signed rank in `ranktransform()` ( #87 ).

- Fixed bug in `standardize()` for standard objects with non-standard
  class-attributes (like vectors of class `haven_labelled` or `vctrs_vctr`).

- Fix `effectsize()` for one sample `t.test(...)` ( #95 ; thanks to pull request
  by @mutlusun )

# effectsize 0.3.1

## New features

- `standardize_parameters()` now returns CIs ( #72 )

- `eta_squared()`, `omega_squared()` and `epsilon_squared()` now works with

  - `gam` models.

  - `afex` models.

  - `lme` and `anova.lme` objects.

- New function `equivalence_test()` for effect sizes.

- New plotting methods in the `see` package.

# effectsize 0.3.0

## New features

- New general purpose `effectsize()` function.

- Effectsize for differences have CI methods, and return a data frame.

- Effectsize for ANOVA all have CI methods, and none are based on
  bootstrapping.

- New effect sizes for contingency tables (`phi()` and `cramers_v()`).

- `chisq_to_phi()` / `cramers_v()` functions now support CIs (via the ncp
  method), and return a data frame.

- `F_to_eta2()` family of functions now support CIs (via the ncp method), and
  return a data frame.

- `t_to_d()` and `t_to_r()` now support CIs (via the ncp method), and return a
  data frame.

- `standardize()` for model-objects has a default-method, which usually accepts
  all models. Exception for model-objects that do not work will be added if
  missing.

- `standardize.data.frame()` gets `append` and `suffix` arguments, to add
  (instead of replace) standardized variables to the returned data frame.

- `eta_squared()`, `omega_squared()` and `epsilon_squared()` now works

  - output from `parameters::model_parameters()`.

  - `mlm` models.

## Bug fixes

- Fix `cohens_d()`'s dealing with formula input (#44).

- `sd_pooled()` now returns the... pooled sd (#44).

## Changes

- In `t_to_d()`, argument `pooled` is now `paired`.

# effectsize 0.2.0

## Bug fixes

- `standardize.data.frame()` did not work when variables had missing values.

- Fixed wrong computation in `standardize()` when `two_sd = TRUE`.

- Fixed bug with missing column names in `standardize_parameters()` for models
  with different components (like count and zero-inflation).

# effectsize 0.1.1

## Changes

- News are hidden in an air of mystery...

# effectsize 0.1.0

## New features

- `standardize_parameters()` and `standardize()` now support models from
  packages *brglm*, *brglm2*, *mixor*, *fixest*, *cgam*, *cplm*, *cglm*,
  *glmmadmb* and *complmrob*.

## Bug fixes

- Fix CRAN check issues.

