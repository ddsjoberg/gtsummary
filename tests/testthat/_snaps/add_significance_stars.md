# works as expected without error

    Code
      tbl1 %>% add_significance_stars(hide_ci = FALSE, hide_p = FALSE) %>%
        as.data.frame()
    Output
        **Characteristic** **Beta** **SE** **95% CI** **p-value**
      1                sex       52   27.9  -2.5, 107       0.061
      2            ph.ecog    -58**   19.1   -96, -21       0.003

---

    Code
      tbl_stars %>% as.data.frame()
    Output
        **Characteristic** **Beta** **SE** **95% CI** **p-value**
      1                sex       52   27.9  -2.5, 107       0.061
      2            ph.ecog    -58**   19.1   -96, -21       0.003

---

    Code
      tbl1 %>% add_significance_stars(thresholds = c(1e-07, 0.55, 0.9, 1), hide_p = FALSE) %>%
        as.data.frame()
    Output
        **Characteristic** **Beta** **SE** **p-value**
      1                sex    52***   27.9       0.061
      2            ph.ecog   -58***   19.1       0.003

