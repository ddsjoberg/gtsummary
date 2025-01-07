# tbl_ard_summary() works

    Code
      as.data.frame(tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM,
      cards::ard_categorical(variables = "AGEGR1"), cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE, .missing = TRUE), by = ARM))
    Output
        **Characteristic**       **Placebo** **Xanomeline High Dose**
      1 Pooled Age Group 1              <NA>                     <NA>
      2              65-80        42 (48.8%)               55 (65.5%)
      3                <65        14 (16.3%)               11 (13.1%)
      4                >80        30 (34.9%)               18 (21.4%)
      5                Age 76.0 (69.0, 82.0)        76.0 (70.5, 80.0)
        **Xanomeline Low Dose**
      1                    <NA>
      2              47 (56.0%)
      3                8 (9.5%)
      4              29 (34.5%)
      5       77.5 (71.0, 82.0)

---

    Code
      as.data.frame(tbl_ard_summary(cards::ard_stack(data = cards::ADSL, cards::ard_categorical(
        variables = "AGEGR1"), cards::ard_continuous(variables = "AGE"), .attributes = TRUE,
      .missing = TRUE, .total_n = TRUE)))
    Output
        **Characteristic**       **Overall**
      1 Pooled Age Group 1              <NA>
      2              65-80       144 (56.7%)
      3                <65        33 (13.0%)
      4                >80        77 (30.3%)
      5                Age 77.0 (70.0, 81.0)

# tbl_ard_summary(cards)

    Code
      as.data.frame(tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM,
      cards::ard_continuous(variables = "AGE"), .attributes = FALSE, .missing = FALSE),
      by = ARM, missing = "no"))
    Output
        **Characteristic**       **Placebo** **Xanomeline High Dose**
      1                AGE 76.0 (69.0, 82.0)        76.0 (70.5, 80.0)
        **Xanomeline Low Dose**
      1       77.5 (71.0, 82.0)

---

    Code
      as.data.frame(tbl_ard_summary(cards::ard_continuous(trial, by = trt, variables = age),
      by = trt))
    Output
        **Characteristic**        **Drug A**        **Drug B**
      1                age 46.0 (37.0, 60.0) 48.0 (39.0, 56.0)

# tbl_ard_summary(cards) error messages

    Code
      tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = c(ARM, AGEGR1),
      cards::ard_continuous(variables = "AGE"), .attributes = TRUE, .missing = TRUE),
      by = ARM)
    Condition
      Error in `tbl_ard_summary()`:
      ! The `cards` object may only contain a single stratifying variable.
      i But contains "group2" and "group2_level".

---

    Code
      tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM, cards::ard_continuous(
        variables = "AGE"), .attributes = FALSE, .missing = FALSE), by = ARM,
      missing = "ifany")
    Condition
      Error in `FUN()`:
      ! Argument `missing = "ifany"` requires results from `cards::ard_missing()`, but they are missing for variable "AGE".
      i Set `missing = "no"` to avoid printing missing counts.

# tbl_ard_summary(by) messaging

    Code
      tbl_ard_summary(cards::bind_ard(cards::ard_continuous(trial, by = trt,
        variables = age), cards::ard_continuous(trial, by = grade, variables = age)),
      by = trt)
    Condition
      Error in `tbl_ard_summary()`:
      ! For `by = "trt"`, columns "group1" and "group1_level" must be present in `cards` and "group1" must be equal to "trt".

---

    Code
      tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM, cards::ard_continuous(
        variables = "AGE"), .attributes = TRUE, .missing = TRUE))
    Condition
      Error in `tbl_ard_summary()`:
      ! The `cards` object may not have group columns when the `by` is empty.

# tbl_ard_summary(statistic) argument works

    Code
      as.data.frame(tbl_ard_summary(ard, statistic = list(all_continuous() ~
        "{median}", all_categorical() ~ "{n} / {N} (Total {N_obs})")))
    Output
        **Characteristic**           **Overall**
      1 Pooled Age Group 1                  <NA>
      2              65-80 144 / 254 (Total 254)
      3                <65  33 / 254 (Total 254)
      4                >80  77 / 254 (Total 254)
      5                Age                  77.0

---

    Code
      as.data.frame(tbl_ard_summary(ard, type = list(all_continuous() ~ "continuous2"),
      statistic = list(all_continuous() ~ c("{median}", "{mean}"))))
    Output
        **Characteristic** **Overall**
      1 Pooled Age Group 1        <NA>
      2              65-80 144 (56.7%)
      3                <65  33 (13.0%)
      4                >80  77 (30.3%)
      5                Age        <NA>
      6             Median        77.0
      7               Mean        75.1

# tbl_ard_summary(type) error messages

    Code
      tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM, cards::ard_continuous(
        variables = "AGE"), .attributes = TRUE, .missing = TRUE), by = ARM, type = list(
        AGE = "categorical"))
    Condition
      Error in `tbl_ard_summary()`:
      ! Summary type for variable "AGE" must be one of "continuous" and "continuous2", not "categorical".
      i If your variable has "variable_level" values, you may need `type = list("AGE"="dichotomous")`.

---

    Code
      tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM, cards::ard_categorical(
        variables = "AGEGR1"), .attributes = TRUE, .missing = TRUE), by = ARM, type = list(
        AGEGR1 = "continuous"))
    Condition
      Error in `tbl_ard_summary()`:
      ! Summary type for variable "AGEGR1" must be "categorical", not "continuous".

# tbl_ard_summary(statistic) error messages

    Code
      tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM, cards::ard_continuous(
        variables = "AGE"), .attributes = TRUE, .missing = TRUE), by = ARM,
      statistic = list(AGE = "{not_a_valid_summary_statistic}"))
    Condition
      Error in `tbl_ard_summary()`:
      ! Statistic "not_a_valid_summary_statistic" is not available for variable "AGE".
      i Select among "N", "mean", "sd", "median", "p25", "p75", "min", "max", "N_obs", "N_miss", "N_nonmiss", "p_miss", and "p_nonmiss".

---

    Code
      tbl_ard_summary(cards::ard_stack(data = cards::ADSL, .by = ARM, cards::ard_continuous(
        variables = "AGE"), .attributes = TRUE, .missing = TRUE), by = ARM,
      statistic = list(AGE = c("{mean}", "{median}")))
    Condition
      Error in `tbl_ard_summary()`:
      ! Variable "AGE" is type `continuous` and `statistic` argument value must be a string of length one.

# tbl_ard_summary() non-standard ARDs (ie not 'continuous', 'categorical', etc)

    Code
      as.data.frame(tbl_ard_summary(ard, by = trt, statistic = ~"{estimate}"))
    Output
        **Characteristic** **Drug A** **Drug B**
      1               time       <NA>       <NA>
      2                 12       90.8       86.3
      3                 24       46.9       41.2
      4                age       47.0       47.4

