# tbl_filter.tbl_hierarchical() error messaging works

    Code
      tbl_filter(data.frame(), t = 10)
    Condition
      Error in `check_class()`:
      ! The `x` argument must be class <gtsummary>, not a data frame.

---

    Code
      tbl_filter(tbl, t = "10")
    Condition
      Error in `tbl_filter()`:
      ! `filter` must be an expression.

---

    Code
      tbl_filter(tbl, t = "10", gt = "yes")
    Condition
      Error in `tbl_filter()`:
      ! `filter` must be an expression.

---

    Code
      tbl_filter(tbl, t = "10", eq = "no")
    Condition
      Error in `tbl_filter()`:
      ! `filter` must be an expression.

---

    Code
      tbl_filter(tbl, t = "10", .stat = "pct")
    Condition
      Error in `tbl_filter()`:
      ! `filter` must be an expression.

