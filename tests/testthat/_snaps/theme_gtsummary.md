# theme_gtsummary_journal('lancet') works

    Code
      with_gtsummary_theme(theme_gtsummary_journal("lancet"), expr = as.data.frame(
        modify_column_hide(add_difference(tbl_summary(trial, by = trt, include = marker,
          label = marker ~ "marker", missing = "no")), c("stat_2"))))
    Message
      Setting theme "The Lancet"
    Output
        **Characteristic** **Drug A**  \nN = 98 **Difference**    **95% CI**
      1             marker   0·84 (0·23 – 1·60)           0·20 -0·05 to 0·44
        **p-value**
      1        0·12

# theme_gtsummary_journal('nejm') works

    Code
      with_gtsummary_theme(theme_gtsummary_journal("nejm"), expr = as.data.frame(
        modify_column_hide(add_difference(tbl_summary(trial, by = trt, include = age,
          label = age ~ "Age", missing = "no")), c("stat_2"))))
    Message
      Setting theme "New England Journal of Medicine"
    Output
        **Characteristic** **Drug A**  \nN = 98 **Difference**  **95% CI**
      1                Age         46 (37 – 60)          -0.44 -4.6 to 3.7
        **p-value**
      1        0.83

# theme_gtsummary_journal('jama') works

    Code
      with_gtsummary_theme(theme_gtsummary_journal("jama"), expr = as.data.frame(
        modify_column_hide(add_difference(tbl_summary(trial, by = trt, include = age,
          label = age ~ "Age", missing = "no")), c("stat_2"))))
    Message
      Setting theme "JAMA"
    Output
        **Characteristic** **Drug A**  \nN = 98 **Difference** **(****95% CI****)**
      1  Age, Median (IQR)         46 (37 – 60)                 -0.44 (-4.6 to 3.7)
        **p-value**
      1        0.83

---

    Code
      with_gtsummary_theme(theme_gtsummary_journal("jama"), expr = as.data.frame(
        tbl_regression(lm(hp ~ am, mtcars))))
    Message
      Setting theme "JAMA"
    Output
        **Characteristic** **Beta** **(95% CI)** **p-value**
      1                 am       -33 (-83 to 16)        0.18

