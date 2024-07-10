# add_significance_stars(x)

    Code
      as.data.frame(tbl_stars)
    Output
        **Characteristic** **Beta** **SE** **95% CI** **p-value**
      1                sex       52   27.9  -2.5, 107       0.061
      2            ph.ecog    -58**   19.1   -96, -21       0.003

---

    Code
      as.data.frame(tbl_merge(list(tbl_stars, tbl_stars)))
    Output
        **Characteristic** **Beta** **SE** **95% CI** **p-value** **Beta** **SE**
      1                sex       52   27.9  -2.5, 107       0.061       52   27.9
      2            ph.ecog    -58**   19.1   -96, -21       0.003    -58**   19.1
        **95% CI** **p-value**
      1  -2.5, 107       0.061
      2   -96, -21       0.003

# add_significance_stars(thresholds)

    Code
      as.data.frame(add_significance_stars(tbl1, thresholds = c(1e-07, 0.55, 0.9, 1),
      hide_p = FALSE))
    Output
        **Characteristic** **Beta** **SE** **p-value**
      1                sex    52***   27.9       0.061
      2            ph.ecog   -58***   19.1       0.003

