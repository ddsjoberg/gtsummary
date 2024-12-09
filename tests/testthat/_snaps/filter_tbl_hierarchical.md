# tbl_filter.tbl_hierarchical() works

    Code
      as.data.frame(tbl)
    Output
         **Sex**  \n    **Race**  \n        **Reported Term for the Adverse Event** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                               Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                                           F              13 (25%)                           18 (45%)                          23 (46%)
      3                                                                       WHITE              10 (21%)                           14 (41%)                          20 (45%)
      4                                                   APPLICATION SITE ERYTHEMA              2 (4.2%)                            5 (15%)                           5 (11%)
      5                                                   APPLICATION SITE PRURITUS              2 (4.2%)                            8 (24%)                          10 (23%)
      6                                                                    ERYTHEMA               6 (13%)                            6 (18%)                           8 (18%)
      7                                                                           M              13 (39%)                           24 (55%)                          17 (50%)
      8                                                                       WHITE              12 (40%)                           22 (55%)                          17 (50%)
      9                                                   APPLICATION SITE ERYTHEMA              1 (3.3%)                           10 (25%)                           7 (21%)
      10                                                  APPLICATION SITE PRURITUS              1 (3.3%)                           12 (30%)                          10 (29%)
      11                                                                  DIARRHOEA               6 (20%)                           3 (7.5%)                          2 (5.9%)
      12                                                                   ERYTHEMA               3 (10%)                            5 (13%)                           6 (18%)

# tbl_filter.tbl_hierarchical() error messaging works

    Code
      tbl_filter(data.frame(), t = 10)
    Condition
      Error in `check_class()`:
      ! The `x` argument must be class <gtsummary>, not a data frame.

---

    Code
      tbl_filter(tbl, t = "10")
    Condition
      Error in `tbl_filter()`:
      ! The `t` argument must be numeric.

---

    Code
      tbl_filter(tbl, t = "10", gt = "yes")
    Condition
      Error in `tbl_filter()`:
      ! The `t` argument must be numeric.

---

    Code
      tbl_filter(tbl, t = "10", eq = "no")
    Condition
      Error in `tbl_filter()`:
      ! The `t` argument must be numeric.

---

    Code
      tbl_filter(tbl, t = "10", .stat = "pct")
    Condition
      Error in `tbl_filter()`:
      ! The `t` argument must be numeric.

