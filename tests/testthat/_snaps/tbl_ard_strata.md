# tbl_ard_strata() works

    Code
      as.data.frame(tbl_ard_strata2(cards::ard_summary(dplyr::filter(cards::ADLB, AVISIT %in% c("Baseline"), PARAMCD %in% c("ALB", "BUN")), strata = PARAM, by = TRTA, variables = AVAL), strata = c(group2,
        group2_level), ~ tbl_ard_summary(.x, by = TRTA, label = list(AVAL = .y)), .combine_with = "tbl_stack", .combine_args = list(group_header = NULL)))
    Output
                  **Characteristic**       **Placebo** **Xanomeline High Dose** **Xanomeline Low Dose**
      1                Albumin (g/L) 39.0 (38.0, 40.0)        41.0 (37.0, 44.0)       39.0 (38.0, 41.0)
      2 Blood Urea Nitrogen (mmol/L)    5.4 (3.9, 6.4)           4.6 (4.3, 5.7)          6.1 (5.0, 7.5)

---

    Code
      as.data.frame(tbl_ard_strata(cards::ard_tabulate(cards::ADSL, by = TRTA, variables = AGEGR1), strata = c(group1, group1_level), ~ tbl_ard_summary(.x), .header = "**Group: {strata}**"))
    Output
        **Characteristic** **Overall** **Overall** **Overall**
      1             AGEGR1        <NA>        <NA>        <NA>
      2              65-80  42 (48.8%)  55 (65.5%)  47 (56.0%)
      3                <65  14 (16.3%)  11 (13.1%)    8 (9.5%)
      4                >80  30 (34.9%)  18 (21.4%)  29 (34.5%)

---

    Code
      as.data.frame(tbl_ard_strata2(cards::ard_tabulate(cards::ADSL, by = c(TRTA, SEX), variables = AGEGR1), strata = c(group1, group1_level, group2, group2_level), ~ modify_header(tbl_ard_summary(.x),
      stat_0 = .y)))
    Output
        **Characteristic** **Placebo, F** **Placebo, M** **Xanomeline High Dose, F** **Xanomeline High Dose, M** **Xanomeline Low Dose, F** **Xanomeline Low Dose, M**
      1             AGEGR1           <NA>           <NA>                        <NA>                        <NA>                       <NA>                       <NA>
      2              65-80     22 (41.5%)     20 (60.6%)                  28 (70.0%)                  27 (61.4%)                 28 (56.0%)                 19 (55.9%)
      3                <65      9 (17.0%)      5 (15.2%)                   5 (12.5%)                   6 (13.6%)                  5 (10.0%)                   3 (8.8%)
      4                >80     22 (41.5%)      8 (24.2%)                   7 (17.5%)                  11 (25.0%)                 17 (34.0%)                 12 (35.3%)

# tbl_ard_strata() messaging

    Code
      tbl_ard_strata2(dplyr::bind_rows(cards::ard_tabulate(cards::ADSL, variables = AGEGR1,
      by = TRTA), cards::ard_tabulate(cards::ADSL, variables = AGEGR1, by = SEX)),
      strata = c(group1, group1_level), ~ tbl_ard_summary(.x))
    Condition
      Error in `tbl_ard_strata2()`:
      ! The "group1" column must be the same value for all rows.
      i The following levels are present: "TRTA" and "SEX"

---

    Code
      tbl_ard_strata2(cards::ard_tabulate(cards::ADSL, variables = AGEGR1, by = TRTA),
      strata = c(group2, group2_level), ~ tbl_ard_summary(.x))
    Condition
      Error in `tbl_ard_strata2()`:
      ! Error processing `strata` argument.
      ! Can't select columns that don't exist. x Column `group2` doesn't exist.
      i Select among columns "group1" and "group1_level"

---

    Code
      tbl_ard_strata2(cards::ard_tabulate(cards::ADSL, variables = AGEGR1), strata = any_of(
        "dddd"), ~ tbl_ard_summary(.x))
    Condition
      Error in `tbl_ard_strata2()`:
      ! No columns were selected in the `strata` argument.

