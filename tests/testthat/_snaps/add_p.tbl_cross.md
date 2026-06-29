# add_p.tbl_cross(source_note) errors properly

    Code
      add_p(tbl_cross(trial, row = stage, col = trt), source_note = NA)
    Condition
      Error in `add_p()`:
      ! Argument `source_note` must be TRUE or FALSE.

---

    Code
      add_p(tbl_cross(trial, row = stage, col = trt), source_note = NULL)
    Condition
      Error in `add_p()`:
      ! The `source_note` argument must be a scalar with class <logical>, not NULL.

