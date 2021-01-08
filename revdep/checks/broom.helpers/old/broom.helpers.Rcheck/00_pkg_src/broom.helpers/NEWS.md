# broom.helpers 1.1.0

* **Minor breaking change:** column `var_type` returned by `tidy_identify_variables()`
  is now equal to `"dichotomous"` for categorical variables with only
  2 levels
* **Minor breaking changes:** for intercepts terms, `tidy_identify_variables()`
  now populates `variable` column by `term` content, instead of `NA` (#66)
* **Minor breaking change:** If the variables can't be identified by 
  `tidy_identify_variables()`, the `variable` column is now populated 
  with the content of the `term` column (#63)
* Exporting select helper utility functions (#65)
    - `.generic_selector()`: makes it easy to create selecting functions like `all_continuous()`.  
    - `.select_to_varnames()`: converts selecting syntax into character varnames
    - `.formula_list_to_named_list()`: takes the formula selecting syntax and converts it to a named list. 
* New selecting functions `all_continuous()`, `all_categorical()`,
  `all_dichotomous()`, `all_contrasts()`, `all_intercepts()` and `all_interaction()` for selecting variables 
  from models (#54)
* Added support for multiple imputation models from the {mice} 
  package. The model passed must be the un-pooled models, and the 
  pooling step included in `tidy_fun=` (#49 @ddsjoberg) 
* New function `tidy_select_variables()` to keep/drop
  selected variables in the output (#45)
* New functions `tidy_add_coefficients_type()` and 
  `model_get_coefficients_type` to get the type of coefficients
  (generic, logistic, Poisson or proportional hazard) used
  by a model (#46)
* `tidy_add_contrasts()` and `model_list_contrasts()` now return an
  additional column `contrasts_type`
* New `no_reference_row` argument for `tidy_add_reference_rows()` (#47)
* New method `model_get_nlevels` to get the number of levels of categorical variables
* New column `var_nlevels` returned by `tidy_identify_variables()`,
  `model_identify_variables()` and `tidy_plus_plus()`
* Categorical terms can now be customized with a pattern taking into account
  term level, reference level and/or variable label, see `model_list_terms_levels()`
  and `categorical_terms_pattern` in `tidy_plus_plus()` and `tidy_add_term_labels` (#61)
* `model_list_terms_levels()` now returns additional columns (`level`, `reference_level`
  `contrasts_type`, `var_label`, `var_levels` and `dichotomous`)
* `model_list_variables()` now returns an additional `var_label` column
* The `exponentiate` argument is now passed to the `tidy_*()`
  functions, as an attribute attached to the tibble, as well as custom labels
  (`variable_labels` and `term_labels`)
* `show_single_row` argument now accepts tidyselect notation (#51 @ddsjoberg)
* `tidy_add_estimate_to_reference_rows()` now relies on `emmeans` for
  sum contrasts, allowing to cover a wider range of models
* Tibbles returned by `tidy_*` functions also inherits of `"broom.helpers"` 
  class (#56)
* `interaction_sep` argument has been added to `tidy_plus_plus()`
* Better management of variables with non standard names (#67)
* `.clean_backticks()` and `.escape_regex()` are now exported
* Bug fix for non standard variable names containing
  a character that would have a special meaning in
  a regular expression (#44)
* Bug fix in `tidy_add_header_rows()` for `nnet::multinom` models:
  label for header rows was missing (#50)
* Bug fix: now `tidy_identify_variables()` correctly identify class "integer"
  for this type of variables (#57)
* Bug fix for `tidy_add_header_rows()` for continuous variables with a non 
  standard name (#70)

# broom.helpers 1.0.0

* Initial version
