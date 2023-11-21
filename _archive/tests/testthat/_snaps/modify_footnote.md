# modify_footnote works

    Code
      tbl_summary %>% modify_footnote(update = starts_with("stat_") ~
        "median (IQR) for continuous variables; n (%) categorical variables") %>%
        as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2            Unknown                  7                   4
      3              Grade               <NA>                <NA>
      4                  I           35 (36%)            33 (32%)
      5                 II           32 (33%)            36 (35%)
      6                III           31 (32%)            33 (32%)

---

    Code
      tbl_summary %>% modify_footnote(label = "Variable Footnote", starts_with(
        "stat_") ~
        "median (IQR) for continuous variables; n (%) categorical variables") %>%
        as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2            Unknown                  7                   4
      3              Grade               <NA>                <NA>
      4                  I           35 (36%)            33 (32%)
      5                 II           32 (33%)            36 (35%)
      6                III           31 (32%)            33 (32%)

---

    Code
      tbl_summary %>% modify_footnote(update = everything() ~ NA) %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2            Unknown                  7                   4
      3              Grade               <NA>                <NA>
      4                  I           35 (36%)            33 (32%)
      5                 II           32 (33%)            36 (35%)
      6                III           31 (32%)            33 (32%)

---

    Code
      glm(response ~ age + grade, trial, family = binomial) %>% tbl_regression(
        exponentiate = TRUE) %>% modify_footnote(ci ~ "CI = Credible Interval",
      abbreviation = TRUE) %>% as.data.frame()
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1                Age   1.02 1.00, 1.04        0.10
      2              Grade   <NA>       <NA>        <NA>
      3                  I   <NA>       <NA>        <NA>
      4                 II   0.85 0.39, 1.85         0.7
      5                III   1.01 0.47, 2.16        >0.9

