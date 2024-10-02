# tbl_hierarchical(data) errors properly

    Code
      tbl_hierarchical()
    Condition
      Error in `tbl_hierarchical()`:
      ! The `data` argument cannot be missing.

---

    Code
      tbl_hierarchical(data = letters)
    Condition
      Error in `tbl_hierarchical()`:
      ! The `data` argument must be class <data.frame>, not a character vector.

---

    Code
      tbl_hierarchical(data = dplyr::tibble())
    Condition
      Error in `tbl_hierarchical()`:
      ! The `hierarchies` argument cannot be missing.

