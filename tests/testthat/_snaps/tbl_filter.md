# tbl_filter.tbl_hierarchical() error messaging works

    Code
      tbl_filter(data.frame(), sum(n) > 10)
    Condition
      Error in `check_class()`:
      ! The `x` argument must be class <gtsummary>, not a data frame.

---

    Code
      tbl_filter(tbl, 10)
    Condition
      Error in `tbl_filter()`:
      ! `filter` must be an expression.

