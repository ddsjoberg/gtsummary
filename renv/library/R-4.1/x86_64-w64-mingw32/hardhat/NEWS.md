# hardhat 0.1.6

* Added a new family of `extract_*()` S3 generics for extracting important
  components from various tidymodels objects. S3 methods will be defined in
  other tidymodels packages. For example, tune will register an
  `extract_workflow()` method to easily extract the workflow embedded within the
  result of `tune::last_fit()`.
  
* A logical `indicators` argument is no longer allowed in
  `default_formula_blueprint()`. This was soft-deprecated in hardhat 0.1.4,
  but will now result in an error (#144).

# hardhat 0.1.5

* `use_modeling_files()` (and therefore, `create_modeling_package()`) now
  ensures that all generated functions are templated on the model name. This
  makes it easier to add multiple models to the same package (#152).

* All preprocessors can now `mold()` and `forge()` predictors to one of three 
  output formats (either tibble, matrix, or `dgCMatrix` sparse matrix) via the
  `composition` argument of a blueprint (#100, #150).

# hardhat 0.1.4

* Setting `indicators = "none"` in `default_formula_blueprint()` no longer
  accidentally expands character columns into dummy variable columns. They
  are now left completely untouched and pass through as characters. When
  `indicators = "traditional"` or `indicators = "one_hot"`, character columns
  are treated as unordered factors (#139).

* The `indicators` argument of `default_formula_blueprint()` now takes character
  input rather than logical. To update:
  
  ```
  indicators = TRUE  -> indicators = "traditional"
  
  indicators = FALSE -> indicators = "none"
  ```
  
  Logical input for `indicators` will continue to work, with a warning, until
  hardhat 0.1.6, where it will be formally deprecated.
  
  There is also a new `indicators = "one_hot"` option which expands all factor
  columns into `K` dummy variable columns corresponding to the `K` levels of
  that factor, rather than the more traditional `K - 1` expansion.

# hardhat 0.1.3

* Updated to stay current with the latest vctrs 0.3.0 conventions.

* `scream()` is now stricter when checking ordered factor levels in new data
  against the `ptype` used at training time. Ordered factors must now have
  _exactly_ the same set of levels at training and prediction time. See
  `?scream` for a new graphic outlining how factor levels are handled (#132).

* The novel factor level check in `scream()` no longer throws a novel level
  warning on `NA` values (#131).

# hardhat 0.1.2

* `default_recipe_blueprint()` now defaults to prepping recipes with
  `fresh = TRUE`. This is a safer default, and guards the user against
  accidentally skipping this preprocessing step when tuning (#122).

* `model_matrix()` now correctly strips all attributes from the result of the
  internal call to `model.matrix()`.

# hardhat 0.1.1

* `forge()` now works correctly when used with a recipe that has a predictor
  with multiple roles (#120).

* Require recipes 0.1.8 to incorporate an important bug fix with `juice()` and
  0-column selections.

# hardhat 0.1.0

* Added a `NEWS.md` file to track changes to the package.
