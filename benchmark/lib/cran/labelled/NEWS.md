# labelled 2.7.0

* a `.values` argument has been added to `set_na_values()` and `set_na_range()`, allowing to pass a list of values
* a `.strict` option has been added to `set_variable_labels()`,
  `set_value_labels()`, `add_value_labels()`,  `remove_value_labels()`,
  `set_na_values()` and `set_na_range()`,
  allowing to pass values for columns not observed in the data (it could be useful
  for using a same list of labels for several data.frame sharing some 
  variables) (#70)
* `copy_labels()` is less restrictive for non labelled vectors, copying variable label
  even if the two vectors are not of the same type (#71)
* a `.strict` option has been added to `copy_labels()` (#71)

# labelled 2.6.0

* `look_for()` has been redesigned:
    - `look_for()` now returns a tibble
    - columns with multiple values for each variable are now stored as named lists
    - a print method has been added for a clearer presentation of results in the console
    - use `lookfor_to_long_format()` to convert results with one row per factor level
      and per value label
    - use `convert_list_columns_to_character()` to convert list columns to 
      simpler character vectors
    - `generate_dictionary()` is an equivalent of `look_for()`
* `set_variable_labels`, `set_value_labels`, `add_value_labels`,
  and `remove_value_labels` now accept "tidy dots" (#67 @psanker)
* new function `names_prefixed_by_values()` to get the names of a vector
  prefixed by their corresponding value

# labelled 2.5.0

* new `.keep_value_labels` argument for `recode.haven_labelled()`
* new `.combine_value_labels` argument for `recode.haven_labelled()` (#61)
* new `drop_unused_value_labels()` method
* an additional `.labels` argument for `set_value_labels()`
* `user_na_to_na` argument has been added to `to_character.haven_labelled()`
* `%>%` is now imported from `dplyr`
* a cheatsheet has been added (#47)
* internal documentation is now using **roxygen2** markdown support

# labelled 2.4.0

* fixes for haven 2.3.0 (#59 by @hadley)
* correct re-export of functions from `haven`
* `update_labelled()` has been improved to allow to reconstruct
  all labelled vectors created with a previous versiob of `haven`

# labelled 2.3.1

* an additional argument `keep_var_label` for `remove_labels()`
* bug fix for `unlabelled()` when applied on a vector
* when using `unclass = TRUE` with `to_factor()`, attributes 
  are not removed anymore

# labelled 2.3.0

* new function `unlabelled()`

# labelled 2.2.2

* bug fix for `look_for()` (#52 by @NoahMarconi)
* bug fix in `val_labels_to_na()` documentation

# labelled 2.2.1

* bug fix for `na_range()` and `na_values()`: variable labels 
  are now preserved (#48, thanks to @mspittler)

# labelled 2.2.0

* new function `copy_labels_from()`, compliant with `dplyr` syntax
* `update_labelled()` is now more strict (#42 by @iago-pssjd)
* new functions `look_for()` and `lookfor()` imported from `questionr` (#44)
* new `unlist` option for `var_label()`
* `tagged_na()` and similar functions are now imported from `haven`

# labelled 2.1.0

* `var_label()`, applied to a data.frame, now accepts a character 
  vector of same length as the number of columns.
* `set_variable_labels` has a new `.labels` argument.
* New `unclass` option in `to_factor()`, to be used when `strict = TRUE` (#36)
* Following `haven` version 2.1.0, it is not mandatory anymore to define a value 
  label before defining a SPSS style missing value. `labelled_spss()`, `na_values()`
  and `na_range()` have been updated accordingly (#37)

# labelled 2.0.2

* `to_factor()` bug fix then applied on a data.frame (#33)

# labelled 2.0.1

* `update_labelled()` bug fix then applied on a data.frame (#31)

# labelled 2.0.0

## BREAKING CHANGE

* Following version 2.0.0 of `haven`, `labelled()` and `labelled_spss()` now
  produce objects with class "haven_labelled" and "haven_labelled_spss", due
  to conflict between the previous "labelled" class and the "labelled" class
  used by `Hmisc`.
* A new function `update_labelled()` could be used to convert data imported
  with an older version of `haven` to the new classes.

## Other changes

* `user_na_to_na` option added to `to_factor()`
* `foreign_to_labelled()` now import SPSS missing values (#27)
* a `strict` argument added to `to_factor()` (#25)
* `remove_attributes()` preserve character vectors (#30)

# labelled 1.1.0

* extend `dplyr::recode()` method to be compatible with labelled vectors.
* `copy_labels()` now copy also `na_range` and `na_values` attributes.
* new method `remove_attributes()`

# labelled 1.0.1

* bug fix: argument `drop_unused_labels` could now be used with `to_factor.data.frame()`
* new labels argument for `to_labelled()` method when applied to a factor
* bug fix: appropriate column names with `data.frame` (#20)

# labelled 1.0.0

* now imports `haven`
* new function to deal with user defined missing values (SPSS style): 
  `na_values()`, `na_range()`, `set_na_values()`, `set_na_values()`,
  `remove_user_na()`, `user_na_to_na()`.
* `remove_labels()` has been updated.

# labelled 0.2.3

* new functions `set_variable_labels()`, `set_value_labels()`, `add_value_labels()` 
  and `remove_value_labels()` compatible with `%>%`.
* new functions `remove_val_labels` and `remove_var_label()`.
* bug fix in `to_character.labelled()` when applied to data frames.


# labelled 0.2.2

* `to_factor()`, `to_character()` and `to_labelled.factor()` now preserves variable label.
* bug fix in `to_factor()` when applied to data frames.

# labelled 0.2.0

* Following evolution of `haven`, `labelled` deosn't support missing values anymore (cf. https://github.com/hadley/haven/commit/4b12ff9d51ddb9e7486966b85e0bcff44992904d)
* New function `to_character()` (cf. https://github.com/larmarange/labelled/commit/3d32852587bb707d06627e56407eed1c9d5a49de)
* `to_factor()` could now be applied to a data.frame (cf. https://github.com/larmarange/labelled/commit/ce1d750681fe0c9bcd767cb83a8d72ed4c5fc5fb)
* If `data.table` is available, labelled attribute are now changed by _reference_ (cf. https://github.com/larmarange/labelled/commit/c8b163f706122844d798e6625779e8a65e5bbf41)
* `zap_labels()` added as a synonym of `remove_labels()`
