# tbl_ard_wide_summary(cards)

    Code
      as.data.frame(tbl_ard_wide_summary(cards::ard_stack(trial, cards::ard_dichotomous(
        variables = response), cards::ard_categorical(variables = grade), .missing = TRUE,
      .attributes = TRUE)))
    Output
        **Characteristic** **n** **%**
      1     Tumor Response    61 31.6%
      2              Grade  <NA>  <NA>
      3                  I    68 34.0%
      4                 II    68 34.0%
      5                III    64 32.0%

---

    Code
      as.data.frame(tbl_ard_wide_summary(cards::ard_stack(trial, cards::ard_dichotomous(
        variables = response), cards::ard_categorical(variables = grade))))
    Output
        **Characteristic** **n** **%**
      1           response    61 31.6%
      2              grade  <NA>  <NA>
      3                  I    68 34.0%
      4                 II    68 34.0%
      5                III    64 32.0%

# tbl_ard_wide_summary(cards) messaging

    Code
      tbl_ard_wide_summary(cards::ard_continuous(trial, by = trt, variables = age))
    Condition
      Error in `tbl_ard_wide_summary()`:
      ! The `cards` object cannot contain grouping variables "group1" and "group1_level".

# tbl_ard_wide_summary(type) messaging

    Code
      tbl_ard_wide_summary(cards::ard_stack(trial, cards::ard_continuous(variables = age),
      .missing = TRUE, .attributes = TRUE), type = age ~ "categorical")
    Condition
      Error in `tbl_ard_wide_summary()`:
      ! The `cards` data frame must contain column "variable_level" with summary types "dichotomous" and "categorical".

---

    Code
      tbl_ard_wide_summary(cards::ard_stack(trial, cards::ard_dichotomous(variables = response),
      cards::ard_continuous(variables = age), .missing = TRUE, .attributes = TRUE))
    Condition
      Error in `tbl_ard_wide_summary()`:
      ! The summary types must all be "continuous" or all be "categorical" and "dichotomous".

