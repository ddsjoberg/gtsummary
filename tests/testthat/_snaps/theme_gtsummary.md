# theme_gtsummary_eda() works

    Code
      as.data.frame(with_gtsummary_theme(theme_gtsummary_eda(), expr = tbl_summary(
        trial, include = c(age, grade))))
    Message
      Setting theme "Exploratory Data Analysis"
    Output
        **Characteristic** **N = 200**
      1                Age        <NA>
      2    Median (Q1, Q3) 47 (38, 57)
      3          Mean (SD)     47 (14)
      4           Min, Max       6, 83
      5            Unknown          11
      6              Grade        <NA>
      7                  I  68 (34.0%)
      8                 II  68 (34.0%)
      9                III  64 (32.0%)

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

# theme_gtsummary_journal('qjecon') works

    Code
      with_gtsummary_theme(theme_gtsummary_journal("qjecon"), expr = as.data.frame(
        tbl_regression(lm(mpg ~ factor(cyl) + hp, mtcars))))
    Message
      Setting theme "The Quarterly Journal of Economics"
    Output
        **Characteristic** **Beta**  \n**(SE)**
      1        factor(cyl)                 <NA>
      2                  4                 <NA>
      3                  6     -6.0**  \n(1.64)
      4                  8     -8.5**  \n(2.33)
      5                 hp     -0.02  \n(0.015)

# check_gtsummary_theme()

    Code
      check_gtsummary_theme(mean)
    Condition
      Warning:
      The `x` argument must be a named list.

---

    Code
      check_gtsummary_theme(list(not_a_theme_element = letters))
    Condition
      Warning:
      The following names of `x` are not accepted theme elemets: "not_a_theme_element".

---

    Code
      check_gtsummary_theme(list(`add_global_p-str:type` = letters))
    Message
      v Looks good!

# with_gtsummary_theme()

    Code
      theme_gtsummary_compact()
    Message
      Setting theme "Compact"
    Code
      with_gtsummary_theme(x = list(`pkgwide-str:theme_name` = "My new theme"), expr = identical(
        1L, 1L), msg_ignored_elements = "The following theme elements are temporarilty overwritten: {.val {elements}}.")
    Message
      The following theme elements are temporarilty overwritten: "pkgwide-str:theme_name".
      Setting theme "My new theme"
      Setting theme "Compact"
    Output
      [1] TRUE
    Code
      reset_gtsummary_theme()

