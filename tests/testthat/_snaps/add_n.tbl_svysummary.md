# add_n.tbl_svysummary() works

    Code
      as.data.frame(add_n(tbl_svysummary(svy_trial, include = c(age, grade, response)),
      statistic = "{N_nonmiss} / {N_obs} ({N_nonmiss_unweighted})", footnote = TRUE,
      last = TRUE))
    Output
        **Characteristic** **N = 200**           **N**
      1                Age 47 (38, 57) 189 / 200 (189)
      2            Unknown          11            <NA>
      3              Grade        <NA> 200 / 200 (200)
      4                  I    68 (34%)            <NA>
      5                 II    68 (34%)            <NA>
      6                III    64 (32%)            <NA>
      7     Tumor Response    61 (32%) 193 / 200 (193)
      8            Unknown           7            <NA>

# add_n.tbl_svysummary(statistic) error messaging

    Code
      add_n(tbl_svysummary(svy_trial, by = trt, include = c(trt, age, grade, response)),
      statistic = "no_curlies")
    Condition
      Error in `add_n()`:
      ! No glue elements found in the `statistic` argument ("no_curlies").
      i Do you need to wrap the statistic name in curly brackets, e.g. "{N_nonmiss}"?

---

    Code
      add_n(tbl_svysummary(svy_trial, by = trt, include = c(trt, age, grade, response)),
      statistic = "{not_a_stat}")
    Condition
      Error in `add_n()`:
      ! The following statistics are not valid for the `statistic` argument: "not_a_stat".
      i Select from "N_nonmiss", "N_obs", "p_nonmiss", "N_miss", "p_miss", "N_miss_unweighted", "N_obs_unweighted", "p_miss_unweighted", "N_nonmiss_unweighted", and "p_nonmiss_unweighted".

