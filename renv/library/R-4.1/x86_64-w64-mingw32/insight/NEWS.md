# insight 0.14.4

## Bug fixes

* Fixed issues due to latest *brms* update.

# insight 0.14.3

## New supported model classes

* `systemfit` (*systemfit*)

## General

* Minor improvements for functions that support printing outputs.

## Changes to functions

* `get_predicted()` gains a new option, `predict = "response"` for binomial models.

* Improved stability of `get_variance()` when computing random-slope-intercept
  correlation with categorical random slopes.

* Improved `get_priors()` for *brms* models.

## Bug fixes

* Fixed issue in `get_data()` for *brms* models with auxiliary parameters.

* Fixed issue in `find_formula()` for *brms* models with auxiliary parameters.

* Fixed issue where `get_data()` for *htest* objects did not always preserve
  factors.

* Fixed issue in `format_table()` for ci-levels with longer fractional part.

# insight 0.14.2

## Changes to functions

* `check_if_installed()` gains a `minimum_version` argument, to check if an
  installed package is not older than the specified version number.

* The `package` argument in `check_if_installed()` is now vectorized, so you
  can check for multiple packages in one function call.

* Value formatting functions (like `format_value()` or `format_ci()`) can now
  round to significant digits using `digits = "signif"`.

## Bug fixes

* Fixed issue in `model_info()` with `stan_polr()` models.

* Fixed issue in `find_parameters()` for *brms* when model contained parameters
  for the priors on sigma.

* Fixed issue in `n_obs()` for `stats4::mle()` models.

* Fixed failing tests due to latest *fixest* update.

* Fixed issues due to latest *epiR* update.

# insight 0.14.1

## New functions

* Added several data management and preparation functions: `data_to_long()`, 
  `data_match()`, `data_relocate()`, `data_restoretype()`, `force_numeric()`.
  
## New supported model classes

* Support for `pgmm` (*plm*)

## Changes to functions

* Improved handling of auxiliary parameters for *stanreg* models.

## Bug fixes

* Stability improvements to `get_predicted()`.

* Fixed issues when accessing information from an `afex_aov` model with an 
  empty `aov` slot (in anticipation for `{afex}` v.1.0.0).

* Fixed issue in `model_info()` for *stanreg* object with non-standard
  model-family specification.

# insight 0.14.0

## General

* Better support for accessing auxiliary parameters (via `get_sigma()` and 
  `get_auxiliary()`, as well as `get_parameters(component = "all")`) for 
  `brmsfit` models.

## New functions

* `get_modelmatrix()` as a robust alternative to `model.matrix()` for
  different model classes.

* `format_message()` to format warnings and messages by adjusting the maximum
  line_length, possibly to the width of the console window.

* `format_string()` to shorten a string to a maximum length.

* `check_if_installed()` to see if the needed package is installed.

## New supported model classes

* Support for `mvord` (*mvord*), `SemiParBIV` (*GJRM*), 
  `selection` (*sampleSelection*)

## Changes to functions

* `find_formula()` now warns when data name is present in formula, since this
  can result in unexpected behaviour in other package functions.

* `model_info()` returns `is_bernoulli = TRUE` for Bernoulli models.

* Add `get_statistic()` for *lavaan* models.

* `get_df()` supports more models/objects.

* `get_sigma()` supports more models/objects.

* `get_sigma()` and `get_deviance()` for `lrm` models (package *rms*) now only
  return one value, sigma or deviance for the model with intercept and predictors.

* `get_deviance()` now works for `glmerMod`, `MixMod` and `glmmTMB` models.

* The behaviour and documentation of the `effects` and `component` arguments,
  in particular for `brmsfit` models, were revised to be more consistent.

* `export_table()` now correctly prints the footer if the input was a list of
  data frames.

## Bug fixes

* Fixed issue (warning) in `get_loglikelihood()` for binomial models with
  non-numeric response variables.

* `find_statistic()` correctly distinguishes t- and z-statistic for *emmGrid*
  objects.

* Fixed issue in `model_info()` for `BGGM` and `mgcv::gam()`.

* Fixed issue in `find_formula()` for `gamlss` models with `random()` function
  in formula.

* Fixed issue with `find_parameters()` for *brmsfit* models when auxiliary
  parameters are directly modelled.

* Fixed issue with `get_parameters()` and `find_parameters()` for 
  multi-group *blavaan* models.

* Fixed issue in `ellipsis_info()` when detecting nested models with 
  poly-terms.

* Fixed issue in `find_response()` for *brmsfit* models that used the
  `resp_thres()` function in the model formula.
  
* Fixed issue in `get_predicted_ci()` for models with rank-deficient 
  model matrix.
  
* Argument `zap_small` in `format_value()` did not work properly over 
  vectorized vectors.

# insight 0.13.2

## General

* `get_predicted()` has be revamped with a new API and a stable output form
  (a vector). In the course of this revision, a new function
  `get_predicted_ci()` to calculate uncertainty intervals for model
  predictions.

* Improved support for `orm` (*rms*).

## New supported model classes

* Support for `svy_vglm` (*svyVGAM*), `mjoint` (*joineRML*),
  `mhurdle` (*mhurdle*), `sarlm` (*spatialreg*), `model_fit` (*tidymodels*)

## New functions

* `is_gam_model()` as a small helper to check if a model is a generalized
  additive model with smooth terms.

## Changes to functions

* Added `iterations` argument to `get_predicted()` to control the
  number of draws returned for Bayesian models.

* `model_info()` now returns `$is_gam` if model is generalized additive model
  with smooth terms.

* `format_table()` and `export_table()` now check for valid input (e.g.,
  non-empty data frame) and give an informative message.

* Improved support for `MixMod` (*GLMMadaptive*) in `get_variance()`.

* Improved `print_parameters()`, to allow more flexibility and better cope with
  different output formats.

* `get_parameters()`, `find_parameters()` and `clean_parameters()` for 
  *emmGrid* and *emm_list* objects were revised and now better match the
  actual parameter names (also for contrasts).

## Bug fixes

* Fixed issue in `get_variance()` for models without intercept.

* Fixed labelling issue in `get_parameters()` and `clean_parameters()` for 
  `blavaan` models.

* `clean_parameters()` for *MCMCglmm* objects did not include random parameters.

* Fixed minor issue with unintended sub-titles for `print_html()`.

* Fixed issue in `get_prior()` for `rstanarm::R2()` priors.

# insight 0.13.1

## General

* Improved handling for GAMs.

## New supported model classes

* Support for `elm`, `eglm` (*eflm*)

## Changes to functions

* `get_residuals(..., weighted = TRUE)` doesn't throw warnings if weights are 1
  (no weights specified).

* `n_parameters()` gains a `only_estimable` argument, to remove non-estimable
  parameters from counting the number of parameters for models with
  rank-deficient model matrix.

* `format_ci()` also gains a `zap_small` argument.

## Bug fixed

* Fix or disable failing tests on Mac OS.

* Fixed issues in `get_variance()` with non-correlated
  random-slope-intercepts for *lme4* models.


# insight 0.13.0

## General

* Roll-back R dependency to R >= 3.4.

## New supported model classes

* Support for `crr` (*cmprsk*), `ergm` (*ergm*), `btergm` (*btergm*), `Rchoice`
  (*Rchoice*), `garch` (*tseries*)

## Changes to functions

* Slightly improved handling of different CI-columns in `format_table()`.

* `model_info()` now returns `$is_leventest` if model is an object returned by
  `car::leveneTest()`.

* `get_parameters()` supports `htest` objects.

## Bug fixes

* `get_varcov()` did not properly remove `NA` from rank-deficient models.

* Fixed issue/warning in `get_data()` for some *htest* objects, where the
  environment was not properly evaluated.

* Fixed issue in `format_table()` with p-value formatting, when input data frame
  contained a column named `"p"`, which was not numeric.

* (Hopefully) fixed issue with failing CRAN checks.

# insight 0.12.0

## Breaking changes

* `format_table()` is an alias for `parameters_table()`, and is no longer
  referring to `export_table()`.

## New supported model classes

* Support for `coxr` (*coxrobust*), `coeftest` (*lmtest*), `ivFixed`
  (*ivFixed*), `ivprobit` (*ivprobit*), `riskRegression` (*riskRegression*).
  `summary.lm`, `lmodel2` (*lmodel2*), improved support for `bamlss`
  (*bamlss*).

## New functions

* Added `get_deviance()` function that returns the model deviance as a robust
  alternative to `stats::deviance()`.

* Added `model_name()` function that returns the model's "name".

* Added `format()` method for `find_formula()` output to flatten it.

* Added `null_as_ones = TRUE` argument to `get_weights()` to return vector of 1s
  instead of `NULL`.

* Added `get_intercept()` as a helper function to easily retrieve the value at
  the intercept.

* Added `get_df()` as a robust alternative to `stats::df.residuals()`.

* Added `get_predicted()` as a robust alternative to `stats::fitted()`.

* Added `get_loglikelihood()` (and its alias `loglikelihood()`) function as a
  robust alternative to `stats::logLik()`.

* Added `get_residuals()` as a robust alternative extract model residuals.

* Added `ellipsis_info()` to specify the nature of ellipsis (`...`) inputs.

* Added `is_nested_models()` to check if multiple regression models are nested
  (decreasing or increasing).

* Added generic `print_html()`, to allow other packages to create tables in HTML
  format (via `export_table()`) when not printing the output to console.

* Added `is_mixed_model()`, to safely check if a model is a mixed effects model.
  This function also works for multivariate response models.

* `n_parameters()` was moved from *parameters* to *insight*.

## Changes to functions

* `find_formula()`, `find_predictor()`, `find_random()` and related functions
  now also return names of random effects from generalized additive mixed models
  (`gamm`, `gamm4`, `stan_gamm4`).

* Added support for more BFBayesFactor objects.

* `model_info()` now returns `$is_xtab` for `chisq.test()` and
  `BayesFactor::contingencyTableBF()`. Furthermore, the `$family` element for
  those objects is set to `"categorical"`.

* `n_obs()` now handles number of observations from models with binomial family
  correctly when these have matrix-columns as response variable.

## Bug fixes

* Fixed issue in `find_statistic()` for *fixest* models, which did not return
  the correct value `"t-statistic"` for `feols()`.

* Fixes inconsistencies in `get_priors()` for (linear) `BFBayesFactor` models.

# insight 0.11.1

## General

* Warnings that formerly were printed using `print_color()` now use `warning()`,
  to better suppress warning messages if required.

## New functions

* `find_smooth()`, to return in particular smooth terms used in a model.

## Changes to functions

* `get_variance()` and `get_variance_random()` gain a `tolerance`-argument, to
  set the tolerance level for singularity checks when computing random effect
  variances.

* `parameters_table()` formats more objects from the *easystats* packages, like
  ROPE-range or `p_rope()`.

* `find_statistic()` now supports models of class *scam*.

* `get_data()` now also supports `htest`-object, where possible.

## Bug fixes

* Fix CRAN check issues.

* `find_formula()` for `stan_gamm4()` now correctly includes random effects.

# insight 0.11.0

## Breaking changes

* `model_info()` now also detects models from `oneway.test()`, `binom.test()`
  `chisq.test()`, `mcnemar.test()` and `prop.test()`. Furthermore,
  `model_info()` better deals with objects from `BFBayesFactor`, and censored
  regression models no longer return `TRUE` for `$is_linear`.

* `format_table()` is going to be renamed in a future update. Please use its
  alias `export_table()`.

## New supported model classes

* Support for `scam` (*scam*), `meta_random` and `meta_fixed` (*metaBMA*), `Glm`
  (*rms*), `ridgelm` (*MASS*), `mediate` (*mediation*). Partial support for
  `mcmc.list` (e.g. *bayesGARCH*)

## New function

* `parameters_table()`, which was moved from package *parameters* to *insight*.
  Note that this function is going to be renamed into `format_table()` in a
  future update.

* `find_offset()`, to find the name of offset-terms.

* Added generics for `display()` and `print_md()`, to allow other packages to
  create tables in other formats when not printing the output to console.

## Changes to functions

* `standardize_names()` tries to be as loyal to the *broom*-naming conventions
  as possible.

* The function of the `brackets`-argument in `format_ci()` was changed. It is
  now also possible to provide a length-two character vector, to define own
  brackets that encompass the CI-values.

* Related to the change in `format_ci()`, the function of the
  `brackets`-argument in `parameters_table()` was changed accordingly.
  Furthermore, `parameters_table()` gains a `preserve_attributes`-argument, to
  preserve any attributes from the input data frame.

* `export_table()` gains several new arguments that allows to create tables in
  markdown-format.

* `print_parameters()` gains a `keep_parameter_column`-argument, to keep
  (default) both the `"Cleaned_Parameter"` and `"Parameter"` columns, or - if
  `FALSE` - use `"Cleaned_Parameter"` as new `"Parameter"` column.

## Bug fixes

### `get_data()`

* Fixed issue in `get_data()` for `MixMod` objects, which were caused due to
  internal changes in *GLMMadaptive*.

* `get_data()` for zero-inflated models from *pscl* did not include the
  offset-term in cases where the offset was defined as argument, not inside the
  model formula.

* Fixed issue in `get_data()` for `coxph` models with survival-objects with
  `event`-argument as response.

* Fixed edge case in `get_data()` for column name of response values that were
  log-transformed using `log(x+1)`.

### Other bug fixes

* Fixed issue with `survreg` models that included `strata()` in their formula.

* Fixed warning in CRAN checks for forthcoming R-devel.

# insight 0.10.0

## New function

* `get_sigma()` to return the residual standard deviation.

* `standardize_names()`, which was moved from package *parameters* to
  *insight*.

## New supported model classes

* Support for `maov` (*stats*), `HLfit` (*spaMM*), preliminary support for
  `margins` (*margins*), `merModList` (*merTools*).

## General

* Better support for (weighted) multivariate response models of class `mlm` for
  functions like `get_varcov()` or `clean_parameters()`.

* Make `find_formula()` work with t-tests from *BayesFactor*.

* Improved handling for *mira* objects.

## Changes to functions

* `format_bf()` gains a `na_reference` argument, to set the "reference" for
  Bayes factor values that are `NA`, and an `exact` argument for returning
  scientific formatted extreme values.

* `format_value()` gains a `zap_small` argument, to prevent scientific printing
  of numbers if these have more decimal places than indicated by `digits`.

* `get_weights()` now also returns `NULL` when all weights were 1.

* `get_parameters()` for *BFBayesFactor* objects gets a `verbose` argument.

* `get_parameters()` for *emmGrid* and *emm_list* objects gets a `summary`
  argument, to either return the full posterior samples or the summarized
  centrality indices for Bayesian models.

* `find_formula()` for `MuMIn::model.avg()` now tries to retrieve the random
  effects part of a formula, when present.

* `get_weights()` gains a `na_rm` argument to remove possible missing values.

## Bug fixes

* Fix issues with one-sample Bayesian t-tests (
  https://github.com/easystats/parameters/issues/297 ).

* Fix issue in `format_value()` that printed `"100%"` as `"1e+02%"`.

* Removed unnecessary white-spaces in `format_ci()` when upper or lower interval
  was larger than 1e+5.

* `has_intercept()` did not work correctly when intercept was removed from
  formula using `-1`.

* `find_terms()` now shows removal of intercept formula using `-1` as term
  `"-1"`.

* Fix issues with `get_statistic()` for *vgam* models.

# insight 0.9.6

## Changes to functions

* `get_data()` now works for models from `afex_aov()`.

* `get_parameters()` returns a more informative message for `BFBayesFactor`
  objects when not the first model is indexed.

* `clean_names()` now also removes `exp()`-pattern.

* `clean_names()` for character-objects now works with "interaction patterns"
  (like `clean_names("scale(a):scale(b)")`).

* `format_bf()` gains a `protect_ratio` argument, to print numbers smaller than
  1 as ratios.

## Bug fixes

* Fix issues in CRAN checks.

* `get_priors()` now works for more complex `BFBayesFactor` objects that have
  multiple custom priors.

# insight 0.9.5

## Breaking changes

* `get_data()` did not always "back-transform" log-transformed or scaled
  variables to return the original values. Now this bug has been fixed, and
  `get_data()` should return all variables on the original scale (as if these
  variables were not transformed), as stated in the docs.

## Bug fixes

* `get_data()` now returns the correct original data for "empty" polynomials
  (i.e. `poly(x, 1)`).

* Fix CRAN check issues due to latest _estimatr_ update.

# insight 0.9.1

## New supported model classes

* Support for `mipo` (*mice*), `lqmm` and `lqm` (*lqmm*). Preliminary support
  for `semLME` (*smicd*), `mle` (*stats4*) and `mle2` (*bbmle*).

## Changes to functions

* `model_info()` returns `$is_meta = TRUE` for *brms*-meta-analysis models.

* Make `find_statistic()` work with `mgcv::bam()`.

* `get_variance()` now also support `truncated_nbinom2()` family from
  *glmmTMB*.

## Bug fixes

* Fixed issue with correctly detecting sigma-parameters in `find_parameters()`
  for multiple-response `brmsfit`-models.

* Fixed issue with `find_formula()` for models from `stan_nlmer()`.

* Fixed issues with `find_terms()` when response variable included a namespace,
  like `survival::Surv()`.

* Fixed issues with `get_priors()` for _stanreg_ models, probably caused by the
  latest update to *rstanarm 2.21.2*.

* Fixed issues in `get_variance()` for *brmsfit* models.

* Fixed some issues around `crq` objects (package *quantreg*).

# insight 0.9.0

## New supported model classes

* `BGGM` (*BGGM*), `metaplus` (*metaplus*), `glht` (*multcomp*), `glmm`
  (*glmm*), improved support for `manova` (*stats*)

## New functions

* Value formatting functions `format_bf()`, `format_pd()`, `format_p()`,
  `format_rope()` and `format_number()` were moved from package *parameters* to
  *insight*.

## Changes to functions

* `get_variance()` now also returns the correlation among random slopes.

* `get_variance()` now also (partially) supports `brmsfit` models.

* `get_parameters()` for models that return (posterior or simulated) samples of
  model parameters gains a `summary`-argument, which - if `TRUE` - returns a
  point-estimate (mean of samples) instead of the full samples.

* `format_p()` returns `"> .999"` for p-values equal to or greater than 0.999.

## Bug fixes

* Fixed issue in `find_formula()` that did not properly work for models with
  random effects in formula (in *lme4* notation), when random effects were in
  between fixed effects parts.

* `get_variance()` did not return variance components for random effects for
  null-models with random slopes.

* Fixed issue with `get_variance()` for `lme`-models with categorical random
  slope.

* Fixed issue that occurred since R 4.0.0 in `find_weights()` when function call
  had no `weights`-argument.

* Fixed issue in `get_data()` for models with `cbind()`-response variables and
  matrix-like variables in the model frame (e.g. when using `poly()`).

* Fixed issues with `PROreg::BBmm()`, due to changes in latest package update.
