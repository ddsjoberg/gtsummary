# workflows 0.2.3

* `workflow()` has gained new `preprocessor` and `spec` arguments for adding
  a preprocessor (such as a recipe or formula) and a parsnip model specification
  directly to a workflow upon creation. In many cases, this can reduce the
  lines of code required to construct a complete workflow (#108).
  
* New `extract_*()` functions have been added that supersede the existing
  `pull_*()` functions. This is part of a larger move across the tidymodels
  packages towards a family of generic `extract_*()` functions. The `pull_*()`
  functions have been soft-deprecated, and will eventually be removed (#106).

# workflows 0.2.2
  
* `add_variables()` now allows for specifying a bundle of model terms through
  `add_variables(variables = )`, supplying a pre-created set of variables with
  the new `workflow_variables()` helper. This is useful for supplying a set
  of variables programmatically (#92).

* New `is_trained_workflow()` for determining if a workflow has already been
  trained through a call to `fit()` (#91).

* `fit()` now errors immediately if `control` is not created by
  `control_workflow()` (#89).

* Added `broom::augment()` and `broom::glance()` methods for trained workflow
  objects (#76).

* Added support for butchering a workflow using `butcher::butcher()`.

* Updated to testthat 3.0.0.

# workflows 0.2.1

* New `.fit_finalize()` for internal usage by the tune package.

# workflows 0.2.0

* New `add_variables()` for specifying model terms using tidyselect expressions
  with no extra preprocessing. For example:
  
  ```
  wf <- workflow() %>%
    add_variables(y, c(var1, start_with("x_"))) %>%
    add_model(spec_lm)
  ```
  
  One benefit of specifying terms in this way over the formula method is to
  avoid preprocessing from `model.matrix()`, which might strip the class of
  your predictor columns (as it does with Date columns) (#34).

# workflows 0.1.3

* A test has been updated to reflect a change in parsnip 0.1.3 regarding how
  intercept columns are removed during prediction (#65).

# workflows 0.1.2

* When using a formula preprocessor with `add_formula()`, workflows now uses
  model-specific information from parsnip to decide whether to expand
  factors via dummy encoding (`n - 1` levels), one-hot encoding (`n` levels), or
  no expansion at all. This should result in more intuitive behavior when
  working with models that don't require dummy variables. For example, if a
  parsnip `rand_forest()` model is used with a ranger engine, dummy variables
  will not be created, because ranger can handle factors directly (#51, #53).

# workflows 0.1.1

* hardhat's minimum required version has been bumped to 0.1.2, as it contains
  an important fix to how recipes are prepped by default.

# workflows 0.1.0

* Added a `NEWS.md` file to track changes to the package.
