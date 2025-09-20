# tbl_ard_strata() works

    Code
      as.data.frame(tbl_ard_strata2(cards::ard_summary(dplyr::filter(cards::ADLB, AVISIT %in% c("Baseline", "Week 12", "Week 24"), PARAMCD %in% c("ALB", "BUN")), strata = PARAM, by = TRTA, variables = AVAL),
      strata = c(group2, group2_level), ~ tbl_ard_summary(.x, by = TRTA, label = list(AVAL = .y)), .combine_with = "tbl_stack", .combine_args = list(group_header = NULL)))
    Output
                  **Characteristic**       **Placebo** **Xanomeline High Dose** **Xanomeline Low Dose**
      1                Albumin (g/L) 39.0 (38.0, 40.0)        41.0 (39.0, 43.0)       40.0 (38.0, 42.0)
      2 Blood Urea Nitrogen (mmol/L)    5.0 (4.3, 6.4)           4.6 (4.3, 6.1)          6.1 (5.0, 7.5)

