# tbl_survfit(x) messaging

    Code
      tbl_survfit(survival::survfit(survival::Surv(ttdeath, death) ~ trt + grade,
      trial), times = c(12, 24))
    Condition
      Error in `tbl_survfit()`:
      ! The `tbl_survfit()` function supports `survival::survfit()` objects with no more than one stratifying variable.
      i The model is stratified by "trt" and "grade".

