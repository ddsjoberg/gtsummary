# tbl_ard_summary() works

    Code
      as.data.frame(tbl_ard_summary(ard_stack(data = ADSL, .by = ARM, ard_categorical(
        variables = "AGEGR1"), ard_continuous(variables = "AGE"), .attributes = TRUE,
      .missing = TRUE)))
    Output
        **Characteristic** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84
      1 Pooled Age Group 1                  <NA>                               <NA>
      2              65-80            42 (48.8%)                         55 (65.5%)
      3                <65            14 (16.3%)                         11 (13.1%)
      4                >80            30 (34.9%)                         18 (21.4%)
      5                Age     76.0 (69.0, 82.0)                  76.0 (70.5, 80.0)
        **Xanomeline Low Dose**  \nN = 84
      1                              <NA>
      2                        47 (56.0%)
      3                          8 (9.5%)
      4                        29 (34.5%)
      5                 77.5 (71.0, 82.0)

---

    Code
      as.data.frame(tbl_ard_summary(ard_stack(data = ADSL, ard_categorical(variables = "AGEGR1"),
      ard_continuous(variables = "AGE"), .attributes = TRUE, .missing = TRUE)))
    Output
        **Characteristic**       **N = 254**
      1 Pooled Age Group 1              <NA>
      2              65-80       144 (56.7%)
      3                <65        33 (13.0%)
      4                >80        77 (30.3%)
      5                Age 77.0 (70.0, 81.0)

# tbl_ard_summary(statistic) argument works

    Code
      as.data.frame(tbl_ard_summary(ard, statistic = list(all_continuous() ~ "{median}",
      all_categorical() ~ "{n} / {N} (Total {N_obs})")))
    Output
        **Characteristic**           **N = 254**
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
        **Characteristic** **N = 254**
      1 Pooled Age Group 1        <NA>
      2              65-80 144 (56.7%)
      3                <65  33 (13.0%)
      4                >80  77 (30.3%)
      5                Age        <NA>
      6             Median        77.0
      7               Mean        75.1

# tbl_ard_summary(cards) error messages

    Code
      tbl_ard_summary(ard_stack(data = ADSL, .by = c(ARM, AGEGR1), ard_continuous(
        variables = "AGE"), .attributes = TRUE, .missing = TRUE))
    Condition
      Error in `tbl_ard_summary()`:
      ! The `cards` object may only contain a single stratifying variable.
      i But contains "group2" and "group2_level".

---

    Code
      tbl_ard_summary(ard_stack(data = ADSL, .by = ARM, ard_continuous(variables = "AGE"),
      .attributes = FALSE, .missing = TRUE))
    Condition
      Error in `tbl_ard_summary()`:
      ! "AGE" does not have associated missing or attributes ARD results.
      i Use `cards::ard_missing()`, `cards::ard_attributes()`, or `cards::ard_stack(.missing=TRUE, .attributes=TRUE)` to calculate needed results.

# tbl_ard_summary(type) error messages

    Code
      tbl_ard_summary(ard_stack(data = ADSL, .by = ARM, ard_continuous(variables = "AGE"),
      .attributes = TRUE, .missing = TRUE), type = list(AGE = "categorical"))
    Condition
      Error in `tbl_ard_summary()`:
      ! Summary type for variable "AGE" must be one of "continuous" and "continuous2", not "categorical".

---

    Code
      tbl_ard_summary(ard_stack(data = ADSL, .by = ARM, ard_categorical(variables = "AGEGR1"),
      .attributes = TRUE, .missing = TRUE), type = list(AGEGR1 = "continuous"))
    Condition
      Error in `tbl_ard_summary()`:
      ! Summary type for variable "AGEGR1" must be "categorical", not "continuous".

# tbl_ard_summary(statistic) error messages

    Code
      tbl_ard_summary(ard_stack(data = ADSL, .by = ARM, ard_continuous(variables = "AGE"),
      .attributes = TRUE, .missing = TRUE), statistic = list(AGE = "{not_a_valid_summary_statistic}"))
    Condition
      Error in `tbl_ard_summary()`:
      ! Statistic "not_a_valid_summary_statistic" is not available for variable "AGE".
      i Select among "p_nonmiss", "p_miss", "N_nonmiss", "N_miss", "N_obs", "max", "min", "p75", "p25", "median", "sd", "mean", and "N".

---

    Code
      tbl_ard_summary(ard_stack(data = ADSL, .by = ARM, ard_continuous(variables = "AGE"),
      .attributes = TRUE, .missing = TRUE), statistic = list(AGE = c("{mean}",
        "{median}")))
    Condition
      Error in `tbl_ard_summary()`:
      ! Variable "AGE" is type `continuous` and `statistic` argument value must be a string of length one.

