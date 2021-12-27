# parsnip 0.1.7

## Model Specification Changes

* A model function (`gen_additive_mod()`) was added for generalized additive models. 

* Each model now has a default engine that is used when the model is defined. The default for each model is listed in the help documents. This also adds functionality to declare an engine in the model specification function. `set_engine()` is still required if engine-specific arguments need to be added. (#513)

* parsnip now checks for a valid combination of engine and mode (#529)

* The default engine for `multinom_reg()` was changed to `nnet`. 

## Other Changes

* The helper functions `.convert_form_to_xy_fit()`, `.convert_form_to_xy_new()`, `.convert_xy_to_form_fit()`, and  `.convert_xy_to_form_new()` for converting between formula and matrix interface are now exported for developer use (#508).

* Fix bug in `augment()` when non-predictor, non-outcome variables are included in data (#510).


# parsnip 0.1.6

## Model Specification Changes

* A new linear SVM model `svm_linear()` is now available with the `LiblineaR` engine (#424) and the `kernlab` engine (#438), and the `LiblineaR` engine is available for `logistic_reg()` as well (#429). These models can use sparse matrices via `fit_xy()` (#447) and have a `tidy` method (#474).

* For models with `glmnet` engines: 

  - A single value is required for `penalty` (either a single numeric value or a value of `tune()`) (#481).
  - A special argument called `path_values` can be used to set the `lambda` path as a specific set of numbers (independent of the value of `penalty`). A pure ridge regression models (i.e., `mixture = 1`) will generate incorrect values if the path does not include zero. See issue #431 for discussion (#486).
  
* The `liquidSVM` engine for `svm_rbf()` was deprecated due to that package's removal from CRAN. (#425)

* The xgboost engine for boosted trees was translating `mtry` to xgboost's `colsample_bytree`. We now map `mtry` to `colsample_bynode` since that is more consistent with how random forest works. `colsample_bytree` can still be optimized by passing it in as an engine argument. `colsample_bynode` was added to xgboost after the `parsnip` package code was written. (#495)

* For xgboost, `mtry` and `colsample_bytree` can be passed as integer counts or proportions, while `subsample` and `validation` should always be proportions. `xgb_train()` now has a new option `counts` (`TRUE` or `FALSE`) that states which scale for `mtry` and `colsample_bytree` is being used. (#461)  

## Other Changes

* Re-licensed package from GPL-2 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/parsnip/issues/462).

* `set_mode()` now checks if `mode` is compatible with the model class, similar to `new_model_spec()` (@jtlandis, #467). Both `set_mode()` and `set_engine()` now error for `NULL` or missing arguments (#503).

* Re-organized model documentation:

   * `update` methods were moved out of the model help files (#479).
   * Each model/engine combination has its own help page. 
   * The model help page has a dynamic bulleted list of the engines with links to the individual help pages. 

* `generics::required_pkgs()` was extended for `parsnip` objects. 

* Prediction functions now give a consistent error when a user uses an unavailable value of `type` (#489)

* The `augment()` method was changed to avoid failing if the model does not enable class probabilities. The method now returns tibbles despite the input data class (#487) (#478)

* xgboost engines now respect the `event_level` option for predictions (#460).  


# parsnip 0.1.5

* An RStudio add-in is available that makes writing multiple `parsnip` model specifications to the source window. It can be accessed via the IDE addin menus or by calling `parsnip_addin()`.

* For `xgboost` models, users can now pass `objective` to `set_engine("xgboost")`. (#403)

* Changes to test for cases when CRAN cannot get `xgboost` to work on their Solaris configuration.

* There is now an `augument()` method for fitted models. See `augment.model_fit`. (#401)

* Column names for `x` are now required when `fit_xy()` is used. (#398)

* There is now an `event_level` argument for the `xgboost` engine. (#420)

* New mode "censored regression" and new prediction types "linear_pred", "time", "survival", "hazard". (#396)

* Censored regression models cannot use `fit_xy()` (use `fit()`). (#442)

# parsnip 0.1.4

* `show_engines()` will provide information on the current set for a model. 

* For three models (`glmnet`, `xgboost`, and `ranger`), enable sparse matrix use via `fit_xy()` (#373).

* Some added protections were added for function arguments that are dependent on the data dimensions (e.g., `mtry`, `neighbors`, `min_n`, etc). (#184)

* Infrastructure was improved for running `parsnip` models in parallel using PSOCK clusters on Windows. 

# parsnip 0.1.3

 * A `glance()` method for `model_fit` objects was added (#325)

 * Specific `tidy()` methods for `glmnet` models fit via `parsnip` were created so that the coefficients for the specific fitted `parsnip` model are returned.

## Fixes

 * `glmnet` models were fitting two intercepts (#349)
 
 * The various `update()` methods now work with engine-specific parameters. 
 
# parsnip 0.1.2

## Breaking Changes

 * `parsnip` now has options to set specific types of predictor encodings for different models. For example, `ranger` models run using `parsnip` and `workflows` do the same thing by _not_ creating indicator variables. These encodings can be overridden using the `blueprint` options in `workflows`. As a consequence, it is possible to get a different model fit that previous versions of `parsnip`. More details about specific encoding changes are below. (#326)

## Other Changes

 * `tidyr` >= 1.0.0 is now required. 
 
 * SVM models produced by `kernlab` now use the formula method (see breaking change notice above). This change was due to how `ksvm()` made indicator variables for factor predictors (with one-hot encodings). Since the ordinary formula method did not do this, the data are passed as-is to `ksvm()` so that the results are closer to what one would get if `ksmv()` were called directly. 
 
 * MARS models produced by `earth` now use the formula method. 
 
 * For `xgboost`, a one-hot encoding is used when indicator variables are created. 
 
 * Under-the-hood changes were made so that non-standard data arguments in the modeling packages can be accommodated. (#315)
 
## New Features

 * A new main argument was added to `boost_tree()` called `stop_iter` for early stopping. The `xgb_train()` function gained arguments for early stopping and a percentage of data to leave out for a validation set. 
 
 * If `fit()` is used and the underlying model uses a formula, the _actual_ formula is pass to the model (instead of a placeholder). This makes the model call better. 
 
 * A function named `repair_call()` was added. This can help change the underlying models `call` object to better reflect what they would have obtained if the model function had been used directly (instead of via `parsnip`). This is only useful when the user chooses a formula interface and the model uses a formula interface. It will also be of limited use when a recipes is used to construct the feature set in `workflows` or `tune`. 
 
 * The `predict()` function now checks to see if required modeling packages are installed. The packages are loaded (but not attached). (#249) (#308) (tidymodels/workflows#45)
 
 * The function `req_pkgs()` is a user interface to determining the required packages.  (#308)
 
# parsnip 0.1.1

## New Features

 * `liquidSVM` was added as an engine for `svm_rbf()` (#300)

## Fixes

* The error message for missing packages was fixed (#289 and #292)


## Other Changes

* S3 dispatch for `tidy()` was broken on R 4.0. 


# parsnip 0.0.5

## Fixes

* A bug ([#206](https://github.com/tidymodels/parsnip/issues/206) and [#234](https://github.com/tidymodels/parsnip/issues/234)) was fixed that caused an error when predicting with a multinomial `glmnet` model. 

## Other Changes

 * `glmnet` was removed as a dependency since the new version depends on 3.6.0 or greater. Keeping it would constrain `parsnip` to that same requirement. All `glmnet` tests are run locally. 
 
 * A set of internal functions are now exported. These are helpful when creating a new package that registers new model specifications. 
 
## New Features

 * `nnet` was added as an engine to `multinom_reg()` [#209](https://github.com/tidymodels/parsnip/issues/209)

## Breaking Changes

 * There were some mis-mapped parameters (going between `parsnip` and the underlying model function) for `spark` boosted trees and some `keras` models. See [897c927](https://github.com/tidymodels/parsnip/commit/897c92719332caf7344e7c9c8895ac673517d2c8).


# parsnip 0.0.4

## New Features

* The time elapsed during model fitting is stored in the `$elapsed` slot of the parsnip model object, and is printed when the model object is printed.

* Some default parameter ranges were updated for SVM, KNN, and MARS models. 

* The model `udpate()` methods gained a `parameters` argument for cases when the parameters are contained in a tibble or list. 

* `fit_control()` is soft-deprecated in favor of `control_parsnip()`. 

## Fixes

* [A bug](https://github.com/tidymodels/parsnip/issues/222) was fixed standardizing the output column types of `multi_predict` and `predict` for `multinom_reg`.

* [A bug](https://github.com/tidymodels/parsnip/issues/208) was fixed related to using data descriptors and `fit_xy()`. 

* A bug was fixed related to the column names generated by `multi_predict()`. The top-level tibble will always have a column named `.pred` and this list column contains tibbles across sub-models. The column names for these sub-model tibbles will have names consistent with `predict()` (which was previously incorrect). See [43c15db](https://github.com/tidymodels/parsnip/commit/43c15db377ea9ef27483ff209f6bd0e98cb830d2).

* [A bug](https://github.com/tidymodels/parsnip/issues/174) was fixed standardizing the column names of `nnet` class probability predictions.


# parsnip 0.0.3.1

Test case update due to CRAN running extra tests [(#202)](https://github.com/tidymodels/parsnip/issues/202)
 

# parsnip 0.0.3

Unplanned release based on CRAN requirements for Solaris.

## Breaking Changes

 * The method that `parsnip` stores the model information has changed. Any custom models from previous versions will need to use the new method for registering models. The methods are detailed in `?get_model_env` and the [package vignette for adding models](https://parsnip.tidymodels.org/articles/articles/Scratch.html).

 * The mode needs to be declared for models that can be used for more than one mode prior to fitting and/or translation. 

 * For `surv_reg()`, the engine that uses the `survival` package is now called `survival` instead of `survreg`.  

 * For `glmnet` models, the full regularization path is always fit regardless of the value given to `penalty`. Previously, the model was fit with passing `penalty` to `glmnet`'s `lambda` argument and the model could only make predictions at those specific values. [(#195)](https://github.com/tidymodels/parsnip/issues/195)

## New Features

 * `add_rowindex()` can create a column called `.row` to a data frame. 
 
 * If a computational engine is not explicitly set, a default will be used. Each default is documented on the corresponding model page. A warning is issued at fit time unless verbosity is zero.  

 * `nearest_neighbor()` gained a `multi_predict` method. The `multi_predict()` documentation is a little better organized.  
 
 * A suite of internal functions were added to help with upcoming model tuning features.  

 * A `parsnip` object always saved the name(s) of the outcome variable(s) for proper naming of the predicted values. 


# parsnip 0.0.2

Small release driven by changes in `sample()` in the current r-devel. 


## New Features

* A "null model" is now available that fits a predictor-free model (using the mean of the outcome for regression or the mode for classification).  

* `fit_xy()` can take a single column data frame or matrix for `y` without error 

## Other Changes

* `varying_args()` now has a `full` argument to control whether the full set
of possible varying arguments is returned (as opposed to only the arguments
that are actually varying).

* `fit_control()` not returns an S3 method. 

* For classification models, an error occurs if the outcome data are not encoded as factors (#115). 

* The prediction modules (e.g. `predict_class`, `predict_numeric`, etc) were de-exported. These were internal functions that were not to be used by the users and the users were using them. 

 * An event time data set (`check_times`) was included that is the time (in seconds) to run `R CMD check` using the "r-devel-windows-ix86+x86_64` flavor. Packages that errored are censored. 

## Bug Fixes

* `varying_args()` now uses the version from the `generics` package. This means
that the first argument, `x`, has been renamed to `object` to align with 
generics.

* For the recipes step method of `varying_args()`, there is now error checking
to catch if a user tries to specify an argument that _cannot_ be varying as 
varying (for example, the `id`) (#132).

* `find_varying()`, the internal function for detecting varying arguments, 
now returns correct results when a size 0 argument is provided. It can also now
detect varying arguments nested deeply into a call (#131, #134).

* For multinomial regression, the `.pred_` prefix is now only added to prediction
column names once (#107).

* For multinomial regression using glmnet, `multi_predict()` now pulls the 
correct default penalty (#108).

* Confidence and prediction intervals for logistic regression were only computed the intervals for a single level. Both are now computed. (#156)


# parsnip 0.0.1

First CRAN release

# parsnip 0.0.0.9005

* The engine, and any associated arguments, are now specified using `set_engine()`. There is no `engine` argument 


# parsnip 0.0.0.9004

* Arguments to modeling functions are now captured as quosures. 
* `others` has been replaced by `...`
* Data descriptor names have beemn changed and are now functions. The descriptor definitions for "cols" and "preds" have been switched. 

# parsnip 0.0.0.9003

* `regularization` was changed to `penalty` in a few models to be consistent with [this change](https://tidymodels.github.io/model-implementation-principles/standardized-argument-names.html#tuning-parameters). 
* If a mode is not chosen in the model specification, it is assigned at the time of fit. [51](https://github.com/tidymodels/parsnip/issues/51)
* The underlying modeling packages now are loaded by namespace. There will be some exceptions noted in the documentation for each model. For example, in some `predict` methods, the `earth` package will need to be attached to be fully operational.

# parsnip 0.0.0.9002

* To be consistent with `snake_case`, `newdata` was changed to `new_data`. 
* A `predict_raw` method was added. 

# parsnip 0.0.0.9001

* A package dependency suffered a new change. 

# parsnip 0.0.0.9000

* The `fit` interface was previously used to cover both the x/y interface as well as the formula interface. Now, `fit()` is the formula interface and [`fit_xy()` is for the x/y interface](https://github.com/tidymodels/parsnip/issues/33). 
* Added a `NEWS.md` file to track changes to the package.
* `predict` methods were [overhauled](https://github.com/tidymodels/parsnip/issues/34) to be [consistent](https://github.com/tidymodels/parsnip/issues/41).
* MARS was added. 
