# parameters 0.14.0

## Breaking changes

* `check_sphericity()` has been renamed into `check_sphericity_bartlett()`.

* Removed deprecated arguments.

* `model_parameters()` for bootstrapped samples used in *emmeans* now treats
  the bootstrap samples as samples from posterior distributions (Bayesian
  models).

## New supported model classes

* `SemiParBIV` (*GJRM*), `selection` (*sampleSelection*), `htest` from the 
  *survey* package, `pgmm` (*plm*).

## General

* Performance improvements for models from package *survey*.

## New functions

* Added a `summary()` method for `model_parameters()`, which is a convenient
  shortcut for `print(..., select = "minimal")`.

## Changes to functions

### `model_parameters()`

* `model_parameters()` gains a `parameters` argument, which takes a regular
  expression as string, to select specific parameters from the returned data 
  frame.

* `print()` for `model_parameters()` and `compare_parameters()` gains a `groups`
  argument, to group parameters in the output. Furthermore, `groups` can be
  used directly as argument in `model_parameters()` and `compare_parameters()`
  and will be passed to the `print()` method.

* `model_parameters()` for ANOVAs now saves the type as attribute and prints
  this information as footer in the output as well.

* `model_parameters()` for *htest*-objects now saves the alternative hypothesis
  as attribute and prints this information as footer in the output as well.

* `model_parameters()` passes arguments `type`, `parallel` and `n_cpus` down
  to `bootstrap_model()` when `bootstrap = TRUE`.

### other

* `bootstrap_models()` for *merMod* and *glmmTMB* objects gains further 
  arguments to set the type of bootstrapping and to allow parallel computing.

* `bootstrap_parameters()` gains the `ci_method` type `"bci"`, to compute
  bias-corrected and accelerated bootstrapped intervals.

* `ci()` for `svyglm` gains a `method` argument.

## Bug fixes

* Fixed issue in `model_parameters()` for *emmGrid* objects with Bayesian models.

* Arguments `digits`, `ci_digits` and `p_digits` were ignored for `print()`
  and only worked when used in the call to `model_parameters()` directly.

# parameters 0.13.0

## General

* Revised and improved the `print()` method for `model_parameters()`.

## New supported model classes

* `blrm` (*rmsb*), `AKP`, `med1way`, `robtab` (*WRS2*), `epi.2by2` (*epiR*),
  `mjoint` (*joineRML*), `mhurdle` (*mhurdle*), `sarlm` (*spatialreg*),
  `model_fit` (*tidymodels*), `BGGM` (*BGGM*), `mvord` (*mvord*)

## Changes to functions

### `model_parameters()`

* `model_parameters()` for `blavaan` models is now fully treated as Bayesian
  model and thus relies on the functions from *bayestestR* (i.e. ROPE, Rhat or
  ESS are reported) .

* The `effects`-argument from `model_parameters()` for mixed models was revised
  and now shows the random effects variances by default (same functionality as
  `random_parameters()`, but mimicking the behaviour from
  `broom.mixed::tidy()`). When the `group_level` argument is set to `TRUE`, the
  conditional modes (BLUPs) of the random effects are shown.

* `model_parameters()` for mixed models now returns an `Effects` column even
  when there is just one type of "effects", to mimic the behaviour from
  `broom.mixed::tidy()`. In conjunction with `standardize_names()` users can get
  the same column names as in `tidy()` for `model_parameters()` objects.

* `model_parameters()` for t-tests now uses the group values as column names.

* `print()` for `model_parameters()` gains a `zap_small` argument, to avoid
  scientific notation for very small numbers. Instead, `zap_small` forces to
  round to the specified number of digits.

* To be internally consistent, the degrees of freedom column for `lqm(m)` and
  `cgam(m)` objects (with *t*-statistic) is called `df_error`.

* `model_parameters()` gains a `summary` argument to add summary information 
  about the model to printed outputs.

* Minor improvements for models from *quantreg*.

* `model_parameters` supports rank-biserial, rank epsilon-squared, and Kendall's
  *W* as effect size measures for `wilcox.test()`, `kruskal.test`, and
  `friedman.test`, respectively.

### Other functions

* `describe_distribution()` gets a `quartiles` argument to include 25th and 75th
  quartiles of a variable.

## Bug fixes

* Fixed issue with non-initialized argument `style` in `display()` for
  `compare_parameters()`.

* Make `print()` for `compare_parameters()` work with objects that have "simple"
  column names for confidence intervals with missing CI-level (i.e. when column
  is named `"CI"` instead of, say, `"95% CI"`).

* Fixed issue with `p_adjust` in `model_parameters()`, which did not work for
  adjustment-methods `"BY"` and `"BH"`.

* Fixed issue with `show_sigma` in `print()` for `model_parameters()`.

* Fixed issue in `model_parameters()` with incorrect order of degrees of
  freedom.

# parameters 0.12.0

## General

* Roll-back R dependency to R >= 3.4.

* Bootstrapped estimates (from `bootstrap_model()` or `bootstrap_parameters()`)
  can be passed to `emmeans` to obtain bootstrapped estimates, contrasts, simple
  slopes (etc) and their CIs.

  * These can then be passed to `model_parameters()` and related functions to
    obtain standard errors, p-values, etc.

## Breaking changes

* `model_parameters()` now always returns the confidence level for as additional
  `CI` column.

* The `rule` argument in `equivalenct_test()` defaults to `"classic"`.

## New supported model classes

* `crr` (*cmprsk*), `leveneTest()` (*car*), `varest` (*vars*), `ergm` (*ergm*),
  `btergm` (*btergm*), `Rchoice` (*Rchoice*), `garch` (*tseries*)

## New functions

* `compare_parameters()` (and its alias `compare_models()`) to show / print
  parameters of multiple models in one table.

## Changes to functions

* Estimation of bootstrapped *p*-values has been re-written to be more
  accurate.

* `model_parameters()` for mixed models gains an `effects`-argument, to return
  fixed, random or both fixed and random effects parameters.

* Revised printing for `model_parameters()` for *metafor* models.

* `model_parameters()` for *metafor* models now recognized confidence levels
  specified in the function call (via argument `level`).

* Improved support for effect sizes in `model_parameters()` from *anova*
  objects.

## Bug fixes

* Fixed edge case when formatting parameters from polynomial terms with many
  degrees.

* Fixed issue with random sampling and dropped factor levels in
  `bootstrap_model()`.

# parameters 0.11.0

## New supported model classes

* `coxr` (*coxrobust*), `coeftest` (*lmtest*), `ivfixed` (*ivfixed*), `ivprobit`
  (*ivprobit*), `riskRegression` (*riskRegression*), `fitdistr` (*MASS*),
  `yuen`, `t1way`, `onesampb`, `mcp1` and `mcp2` (*WRS2*), `Anova.mlm` (*car*),
  `rqs` (*quantreg*), `lmodel2` (*lmodel2*), `summary.lm`, `PMCMR`, `osrt` and
  `trendPMCMR` (*PMCMRplus*), `bamlss` (*bamlss*).

## New functions

### Printing and table Formatting

* `print_html()` as an alias for `display(format = "html")`. This allows to
  print tabular outputs from data frames (as returned by most functions in
  _parameters_) into nicely rendered HTML markdown tables.

## Changes to functions

* Added more effect size measures to `model_parameters()` for `htest` objects.

* `model_parameters()` for anova objects gains a `power` argument, to calculate
  the power for each parameter.

* `ci()` for models from *lme4* and *glmmTMB* can now computed profiled
  confidence intervals, using `method = "profile"`. Consequently,
  `model_parameters()` with `df_method = "profile"` also computes profiled
  confidence intervals. For models of class `glmmTMB`, option `"uniroot"` is
  also available.

## Bug fixes

* `model_parameters()` for t-tests when `standardize_d = TRUE`, did not return
  columns for the group-specific means.

* Fixed issue in `p_value()` for `fixest::feols()`.

* Fixed issue in `model_parameters()` for `glmer()` models with p-values that
  were calculated with `df_method = "ml1"` or `df_method = "betwithin"`.

* Fixed issue in `model_parameters()` for multinomial models when response was a
  character vector (and no factor).

* Fixed issue in `print_md()` for model-parameters objects from Bayesian
  models.

* Fixed issues with printing of model parameters for multivariate response
  models from *brms*.

* Fixed issue with paired t-tests and `model_parameters()`.

# parameters 0.10.1

## New functions

* `format_p_adjust()`, to create pretty names for p-adjustment methods.

## Bug fixes

* Fixed breaking code / failing tests due to latest _effectsize_ update.

* Fixed issue with `model_parameters()` for models of class `mlm`.

* Undocumented arguments `digits`, `ci_digits` and `p_digits` worked for
  `print()`, but not when directly called inside `model_parameters()`. Now,
  `model_parameters(model, digits = 5, ci_digits = 8)` works again.

* Fixed some minor printing-issues.

# parameters 0.10.0

## Breaking changes

* The default-method for effect sizes in `model_parameters()` for Anova-models
  (i.e. when arguments `omega_squared`, `eta_squared` or `epsilon_squared` are
  set to `TRUE`) is now `"partial"`, as initially intended.

* Column names for degrees of freedom were revised. `"df_residual"` was replaced
  by the more generic `"df_error"`. Moreover, models of class `htest` now also
  have the column name `"df_error"` and no longer `"df"` (where applicable).

* Some re-exports for functions that were moved to *insight* longer ago, were
  now removed.

## New supported model classes

* `Glm` (*rms*), `mediate` (*mediation*).

* `model_parameters()` supports `Gam` models (*gam*), `ridgelm` (*MASS*),
  `htest` objects from `oneway.test()`, `chisq.test()`, `prop.test()`,
  `mcnemar.test()` and `pairwise.htest` objects, `mcmc.list` (e.g. from
  *bayesGARCH*).

## New functions

### Printing and table Formatting

* `display()`, to format output from package-functions into different formats.

* `print_md()` as an alias for `display(format = "markdown")`. This allows to
  print tabular outputs from data frames (as returned by most functions in
  _parameters_) into nicely rendered markdown tables.

* `format()`, to create a "pretty data frame" with nicer column names and
  formatted values. This is one of the worker-functions behind `print()` or
  `print_md()`.

## Changes to functions

### `model_parameters()`

* `model_parameters()` for Anova-models (of class `aov`, `anova` etc.) gains a
  `ci`-argument, to add confidence intervals to effect size parameters.

* `model_parameters()` for `htest` objects gains a `cramers_v` and `phi`
  argument, to compute effect size parameters for objects from `chisq.test()`,
  and a `standardized_D` argument, to compute effect size parameters for objects
  from `t.test()`.

* `model_parameters()` for `metafor`-models is more stable when called from
  inside functions.

* `model_parameters()` for *metaBMA*-models now includes prior information for
  the meta-parameters.

* `model_parameters()` for meta-analysis-models gains a
  `include_studies`-argument, to include or remove studies from the output.

* `model_parameters()` for gam-models now includes the residual df for smooth
  terms, and no longer the reference df.

* Slightly revised and improved the `print()` method for `model_parameters()`.

### Other functions

* `describe_distribution()` now includes the name of the centrality index in the
  `CI`-column, when `centrality = "all"`.

* `pool_parameters()` gains a `details`-argument. For mixed models, and if
  `details = TRUE`, random effect variances will also be pooled.

## Bug fixes

* Fixed issue in `ci()` for *lme* models with non-positive definite
  variance-covariance.

* Fixed issue in `model_parameters()` for `nnet::multinom()`, `lqmm::lqm()`,
  `mgcv::gam()`, and `margins::margins()` models, and models from package
  *blme*.

# parameters 0.9.0

## New supported model classes

* Support for `maov` (*stats*), `HLfit` (*spaMM*), `scam` (*scam*), preliminary
  support for `emm_list` (*emmeans*), `merModList` (*merTools*), `meta_random`,
  `meta_bma` and `meta_fixed` (*metaBMA*).

## New functions

* `pool_parameters()`, to pool parameters estimates from multiple models.

* `degroup()`, as a more generic case for `demean()`.

* `center()`, to center variables.

## General

* Better support for (weighted) multivariate response models of class `mlm` for
  functions like `model_parameters()` or `simulate_parameters()`.

* `standardize_names()` is now re-exported from the *insight* package.

## Changes to functions

### Printing model parameters

* `print()` for `model_parameters()` now names the coefficients column depending
  on the model type (i.e. `"Odds Ratios"` for logistic regression when
  `exponentiate = TRUE` etc.)

* `print()` for `model_parameters()` gains a `show_sigma` argument, to show or
  hide information on the residual standard deviation.

* `print()` for `model_parameters()` displays a message for Bayesian models,
  indicating which method to compute credible intervals was used.

### Other changes

* `data_partition()` gets a `seed` argument, to explicitly set the seed before
  random sampling of test and training data.

* Revised `parameters_table()`, to improve readability of printed output.

## Bug fixes

* Fixed issues in `model_parameters()` for *vgam* and *mira* objects.

* Fixed issue where `model_parameters()` for *emmGrid* objects falsely removed
  the `Coefficient` column.

* Fixed issue in `parameters_type()` for factors with different effects-coding
  than treatment contrasts.

* Fixed issues due to latest _effectsize_ update.

# parameters 0.8.6

## Bug fixes

* Fixed issues with *glmmTMB* models with dispersion-parameter.

* Fixed issue where `model_parameters()` for *glmmTMB* models falsely removed
  the `Component` column.

* Fixed issue with missing CI columns in `model_parameters()` when `standardize`
  was one of the options except `"refit"`.

* `parameters_type()` did not correctly detect interaction terms for specific
  patterns like `scale()` included in the interaction.

# parameters 0.8.5

## General

* Added vignette on model parameters and missing data.

* Update citation.

## New supported model classes

* Support for `mipo` (*mice*), `lqm` and `lqmm` (*lqmm*). Preliminary support
  for `semLME` (*smicd*), `mle2` (*bbmle*), `mle` (*stats4*)

* `model_parameters()` for objects of class `mira` (*mice*).

## Changes to functions

* `model_parameters()` gets a specific behaviour for brms-meta-analysis models.

* `model_parameters()` for *lavaan* and *blavaan* now also prints self-defined
  parameters.

* `model_parameters()` for *lavaan* and *blavaan* gains more option for
  standardized parameters.

## Bug fixes

* Fix issue in `model_parameters()` for `coxph.penal` models.

* Fix issue in `model_parameters.metaplus()` with random effects.

* Fix issue in `check_heterogeneity()` when `x` was a mixed model.

* Fix issue in `check_heterogeneity()` for data with missing values.

* Fix issue in `dof_ml1()` when random-effect terms where character vectors.

* Fix issue in `print()` method for `model_parameters()` that printed empty
  lines for rows with complete missing values. Empty lines are now removed.

* Fix issue in `parameters_type()` when `exp()` was used in a model formula.

# parameters 0.8.2

## New supported models

* `metaplus` (*metaplus*), `glht` (*multcomp*), `glmm` (*glmm*), `manova`
  (*stats*), `crq` and `crqs` (*quantreg*)

* Improved support for models from the *rms* package.

## Changes to functions

* Improved parameters formatting for ordered factors in `model_parameters()`
  (and `format_parameters()`).

* Argument `df_method` can now also be applied to GLMs, to allow calculation of
  confidence intervals based on Wald-approximation, not profiled confidence
  intervals. This speeds up computation of CIs for models fit to large data
  sets.

* Improved `select_parameters()` for mixed models, and revised docs and
  associated vignette.

## Bug fixes

* Allow `threshold` to be passed to `efa_to_cfa()` when the model is from
  `factor_analysis()`.

* Allow correlation matrix to be passed to `factor_analysis()`.

* Fix CRAN check issues.

* Fix issue in `model_parameters()` for models with non-estimable parameters or
  statistics.

* Fix issue in `model_parameters()` for *plm* models with only one parameter.

* Fix issue in `check_heterogeneity()` in case no predictor would cause
  heterogeneity bias.

* Make sure *clubSandwich* is used conditionally in all places, to properly pass
  CRAN checks.

# parameters 0.8.0

## New supported models

* `robmixglm` (*robmixglm*), `betaor`, `betamfx`, `logitor`, `poissonirr`,
  `negbinirr`, `logitmfx`, `probitmfx`, `poissonmfx`, `negbinmfx` (*mfx*),
  partial support `emmGrid` (*emmeans*)

## Changes to functions

### `simulate_parameters()` and `simulate_model()`

* has a nicer `print()` method.

* now also simulate parameters from the dispersion model for *glmmTMB* objects.

* gets a `verbose` argument, to show or hide warnings and messages.

## Bug fixes

* fix issue with rank deficient models.

# parameters 0.7.0

## General

* We changed the computation of confidence intervals or standard errors, so
  these are now based on a t-distribution with degrees of freedom and not normal
  distribution assuming infinite degrees of freedom. This was implemented for
  most functions before and only affects few functions (like
  `equivalence_test()` or CIs for standardized parameters from
  `model_parameters()` when standardization method was `"posthoc"`).

## New supported models

* `averaging` (*MuMIn*), `bayesx` (*R2BayesX*), `afex_aov` (*afex*)

## New functions

* `check_heterogeneity()` as a small helper to find variables that have a
  within- and between-effect related to a grouping variable (and thus, may
  result in heterogeneity bias, see [this
  vignette](https://easystats.github.io/parameters/articles/demean.html)).

## Changes to functions

### `equivalence_test()`

* gains a `rule` argument, so equivalence testing can be based on different
  approaches.

* for mixed models gains an `effect` argument, to perform equivalence testing on
  random effects.

* gains a `p_values` argument, to calculate p-values for the equivalence test.

* now supports more frequentist model objects.

### `describe_distribution()`

* now works on grouped data frames.

* gains `ci` and `iterations` arguments, to compute confidence intervals based
  on bootstrapping.

* gains a `iqr` argument, to compute the interquartile range.

* `SE` column was removed.

### `model_parameters()`

* `model_parameters()` for Stan-models (*brms*, *rstanarm*) gains a
  `group_level` argument to show or hide parameters for group levels of random
  effects.

* Improved accuracy of confidence intervals in `model_parameters()` with
  `standardize = "basic"` or `standardize = "posthoc"`.

* `model_parameters.merMod()` no longer passes `...` down to bootstrap-functions
  (i.e. when `bootstrap = TRUE`), as this might conflict with
  `lme4::bootMer()`.

* For ordinal models (like `MASS::polr()` or `ordinal::clm()`), a `Component`
  column is added, indicating intercept categories (`"alpha"`) and estimates
  (`"beta"`).

* The `select`-argument from `print.parameters_model()` now gets a
  `"minimal"`-option as shortcut to print coefficients, confidence intervals and
  p-values only.

### Other changes

* `parameters_table()` and `print.parameters_model()` now explicitly get
  arguments to define the digits for decimal places used in output.

* `ci()`, `standard_error()`, `p_value()` and `model_parameters()` for *glmmTMB*
  models now also works for dispersion models.

## Bug fixes

* Fixed issue in `equivalence_test()` for mixed models.

* Fixed bug for `model_parameters.anova(..., eta_squared = "partial")` when
  called with non-mixed models.

* Fixed issue with wrong degrees of freedom in `model_parameters()` for *gam*
  models.

* Fixed issue with unused arguments in `model_parameters()`.

# parameters 0.6.1

## General

* Remove 'Zelig' from suggested packages, as it was removed from CRAN.

## Changes to functions

### model_parameters()

* `model_parameters()` now also transforms standard errors when `exponentiate =
  TRUE`.

* `model_parameters()` for `anova()` from mixed models can now also compute
  effect sizes like eta squared.

* `model_parameters()` for `aov()` gains a `type`-argument to compute type-1,
  type-2 or type-3 sums of squares.

* `model_parameters()` for Bayesian models gains a `standardize` argument, to
  return standardized parameters from the posterior distribution.

* Improved `print()` method for `model_parameters()` for nested `aov()`
  (repeated measurements).

* You can now control whether `demean()` should add attributes to indicate
  within- and between-effects. This is only relevant for the `print()`-method of
  `model_parameters()`.

## Bug fixes

* Fixed `model_parameters()` for `anova()` from *lmerTest* models.

# parameters 0.6.0

## Breaking changes

- Alias `model_bootstrap()` was removed, please use `bootstrap_model()`.

- Alias `parameters_bootstrap()` was removed, please use
  `bootstrap_parameters()`.

- Alias `model_simulate()` was removed, please use `simulate_model()`.

- Alias `parameters_simulate()` was removed, please use
  `simulate_parameters()`.

- Alias `parameters_selection()` was removed, please use `select_parameters()`.

- Alias `parameters_reduction()` was removed, please use `reduce_parameters()`.

- Functions `DDR()`, `ICA()` and `cmds()` are no longer exported, as these were
  intended to be used internally by `reduce_parameters()` only.

- `skewness()` and `kurtosis()` always return a data frame.

## New supported models

- Added support for `arima` (*stats*), `bife` (*bife*), `bcplm` and `zcpglm`
  (*cplm*)

## Changes to functions

### model_parameters()

- Improved print-method for `model_parameters.brmsfit()`.

- Improved print-method for `model_parameters.merMod()` when fitting REWB-Models
  (see `demean()`).

- Improved efficiency for `model_parameters()` (for linear mixed models) when
  `df_method = "kenward"`.

- `model_parameters()` gets a `p_adjust`-argument, to adjust p-values for
  multiple comparisons.

- Minor improvements for `cluster_analysis()` when `method = "kmeans"` and
  `force = TRUE` (factors now also work for kmeans-clustering).

### p_value(), ci() and standard_error()

- `p_value_kenward()`, `se_kenward()` etc. now give a warning when model was not
  fitted by REML.

- Added `ci()`, `standard_error()` and `p_value()` for *lavaan* and *blavaan*
  objects.

- Added `standard_error()` for *brmsfit* and *stanreg* objects.

### Other changes

- Run certain tests only locally, to reduce duration of CRAN checks.

- `skewness()`, `kurtosis()` and `smoothness()` get an `iteration` argument, to
  set the numbers of bootstrap replicates for computing standard errors.

- Improved print-method for `factor_analysis()`.

- `demean()` now additionally converts factors with more than 2 levels to
  dummy-variables (binary), to mimic *panelr*-behaviour.

## Bug fixes

- Fixed minor issue with the `print()`-method for `model_parameters.befa()`.

- Fixed issues in `model_parameters()` (for linear mixed models) with wrong
  order of degrees of freedom when `df_method` was different from default.

- Fixed issues in `model_parameters()` (for linear mixed models) with accuracy
  of p-values when `df_method = "kenward`.

- Fixed issues in `model_parameters()` with wrong test statistic for
  *lmerModLmerTest* models.

- Fixed issue in `format_parameters()` (which is used to format output of
  `model_parameters()`) for factors, when variable name was also part of factor
  levels.

- Fixed issue in `degrees_of_freedem()` for *logistf*-models, which
  unintentionally printed the complete model summary.

- Fixed issue in `model_parameters()` for *mlm* models.

- Fixed issue in `random_parameters()` for uncorrelated random effects.

