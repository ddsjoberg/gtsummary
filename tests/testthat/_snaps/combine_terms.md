# combine_terms catches expected errors

    Code
      combine_terms(tbl_regression(lm(age ~ marker + stage, trial)), formula = . ~ . -
        marker)
    Condition
      Error in `combine_terms()`:
      ! There was an error calculating the combined terms p-value.
      i There are two common causes for an error. See error message below.
      * The model type is not supported by `stats::anova()`.
      * The number of observations used to estimate the full and reduced models is different.
      x models were not all fitted to the same size of dataset

---

    Code
      combine_terms(tbl_regression(lm(age ~ marker + stage, trial)), formula = . ~ . -
        marker, label = c("marker", "marker2"))
    Condition
      Error in `combine_terms()`:
      ! The `label` argument must be a string or empty, not a character vector.

---

    Code
      combine_terms(tbl_regression(lm(mpg ~ disp + am * factor(cyl), data = mtcars)),
      . ~ . - am)
    Condition
      Error in `combine_terms()`:
      ! The output from `anova()` did not contain a p-value.
      i This may happen when there is no default method. Use `test` argument to specify the method, e.g. `test = "LRT"`.

