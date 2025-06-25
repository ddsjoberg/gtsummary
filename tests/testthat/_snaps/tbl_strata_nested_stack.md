# tbl_strata_nested_stack() messaging

    Code
      tbl_strata_nested_stack(dplyr::rename(trial, strata = trt), strata = strata,
      ~ modify_header(tbl_summary(.x, include = c(age, grade), missing = "no"),
      all_stat_cols() ~ "**Summary Statistics**"))
    Condition
      Error in `tbl_strata_nested_stack()`:
      ! At least one column must be selected in the `strata` argument, and columns cannot be named "stat", "stat_name", "n", "N", "p", and "strata".

---

    Code
      tbl_strata_nested_stack(mtcars, strata = am, ~ as_gtsummary(.x))
    Condition
      Error in `tbl_strata_nested_stack()`:
      ! The first column printed must be <character>, which is not the case for table 1 and column "mpg"

---

    Code
      tbl_strata_nested_stack(trial, strata = trt, function(.x) {
        tbl_strata_nested_stack(.x, strata = grade, function(.y) {
          modify_header(tbl_summary(.y, include = age, missing = "no"), all_stat_cols() ~
            "**Summary Statistics**")
        })
      })
    Condition
      Error in `tbl_strata_nested_stack()`:
      ! The `tbl_strata_nested_stack()` function can only be run once on a table. One of the tables already contains a column named "tbl_indent_id1" indicating the function was previously executed.

