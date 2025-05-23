# add_overall.tbl_summary() messaging

    Code
      tbl <- add_overall(tbl_summary(mtcars))
    Message
      Cannot add an overall column with `add_overall()` when original table is not statified with `tbl_summary(by)`.
      i Returning table unaltered.

---

    Code
      add_overall(add_stat_label(tbl_summary(mtcars, by = am, include = "mpg", type = all_continuous() ~
        "continuous2"), label = mpg ~ "UPDATED!"))
    Condition
      Error in `add_overall()`:
      ! An error occured in `add_overall()`, and the overall statistic cannot be added.
      Have variable labels changed since the original call to `tbl_summary()`?

