# add_p.tbl_summary() error messaging with bad inputs

    Code
      add_p(tbl_summary(trial[c("trt", "age")]))
    Condition
      Error in `add_p()`:
      ! Cannot run `add_p()` when `tbl_summary(by)` argument not included.

---

    Code
      add_p(tbl_summary(trial[c("trt", "age")], by = trt), test = list(age = function(
        ...) mtcars))
    Condition
      Error in `add_p()`:
      ! Cannot run `add_p()` when `tbl_summary(by)` argument not included.

---

    Code
      add_p(tbl_summary(trial[c("trt", "age")], by = trt), test = list(age = function(
        ...) letters))
    Condition
      Error in `add_p()`:
      ! Cannot run `add_p()` when `tbl_summary(by)` argument not included.

# add_p.tbl_summary() can be run after add_difference()

    Code
      add_p(add_p(tbl_summary(select(trial, age, trt), by = trt)))
    Condition
      Error in `add_p()`:
      ! Cannot run `add_p()` when `tbl_summary(by)` argument not included.

