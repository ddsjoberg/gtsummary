# modify_column_merge() messaging

    Code
      modify_column_merge(mod, pattern = "{not_a_column} ({conf.low}, {conf.high})",
        rows = !is.na(estimate))
    Condition
      Error in `modify_column_merge()`:
      ! Columns specified in the `modify_column_merge(pattern)` argument are not present in table.
      Columns "not_a_column" not found.

---

    Code
      modify_column_merge(tbl_regression(lm(mpg ~ factor(am), mtcars)), rows = !is.na(
        conf.low), pattern = "{conf.low}:::{not_in_table}")
    Condition
      Error in `modify_column_merge()`:
      ! Columns specified in the `modify_column_merge(pattern)` argument are not present in table.
      Columns "not_in_table" not found.

