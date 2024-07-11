# add_overall.tbl_svysummary() errors

    Code
      add_overall(tbl_svysummary(svy_mtcars))
    Condition
      Error in `add_overall()`:
      ! Cannot run `add_overall()` when original table function is not statified with `tbl_svysummary(by)`.

---

    Code
      add_overall(add_stat_label(tbl_svysummary(svy_mtcars, by = am, include = "mpg",
        type = all_continuous() ~ "continuous2"), label = mpg ~ "UPDATED!"))
    Condition
      Error in `add_overall()`:
      ! An error occured in `add_overall()`, and the overall statistic cannot be added.
      Have variable labels changed since the original call to `tbl_svysummary()`?

