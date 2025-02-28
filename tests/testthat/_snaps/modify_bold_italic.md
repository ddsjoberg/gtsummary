# remove_bold/italic() removes all by default

    Code
      as.data.frame(remove_bold(bold_p(tbl_regression(glm(response ~ death, trial,
      family = binomial())))), col_labels = FALSE)
    Output
               label estimate    conf.low p.value
      1 Patient Died    -0.96 -1.6, -0.34   0.003

---

    Code
      as.data.frame(remove_italic(modify_italic(tbl_summary(trial, include = age,
        missing = "no"), columns = label, rows = variable == "age")), col_labels = FALSE)
    Output
        label      stat_0
      1   Age 47 (38, 57)

