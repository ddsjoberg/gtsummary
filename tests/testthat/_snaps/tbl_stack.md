# tbl_stack throws expected errors

    Code
      tbl_stack(t1_summary, t2_summary)
    Condition
      Error in `tbl_stack()`:
      ! The `tbls` argument must be class <list>, not a <tbl_summary> object.

---

    Code
      tbl_stack(list(mtcars))
    Condition
      Error in `tbl_stack()`:
      ! Each element of the list `tbls` must be class <gtsummary>.

---

    Code
      tbl_stack(tbls = list(t1_summary, t2_summary), group_header = c("Table"))
    Condition
      Error in `tbl_stack()`:
      ! The `group_header` argument must be length 2 or empty.

