# add_n.tbl_summary(statistic) error messaging

    Code
      add_n(tbl_summary(trial, by = trt, include = c(trt, age, grade, response)),
      statistic = "no_curlies")
    Condition
      Error in `add_n()`:
      ! No glue elements found in the `statistic` argument ("no_curlies").
      i Do you need to wrap the statistic name in curly brackets, e.g. "{N_nonmiss}"?

---

    Code
      add_n(tbl_summary(trial, by = trt, include = c(trt, age, grade, response)),
      statistic = "{not_a_stat}")
    Condition
      Error in `add_n()`:
      ! The following statistics are not valid for the `statistic` argument: "not_a_stat".
      i Select from "N_obs", "N_miss", "N_nonmiss", "p_miss", and "p_nonmiss".

