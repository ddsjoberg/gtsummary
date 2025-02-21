# tbl_filter.tbl_hierarchical() works

    Code
      as.data.frame(tbl)
    Output
         **Sex**  \n    **Race**  \n        **Reported Term for the Adverse Event** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                               Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                                           F              13 (25%)                           18 (45%)                          23 (46%)
      3                                                   BLACK OR AFRICAN AMERICAN               3 (60%)                            4 (67%)                           3 (50%)
      4                                                                       WHITE              10 (21%)                           14 (41%)                          20 (45%)
      5                                                   APPLICATION SITE PRURITUS              2 (4.2%)                            8 (24%)                          10 (23%)
      6                                                                    ERYTHEMA               6 (13%)                            6 (18%)                           8 (18%)
      7                                                   APPLICATION SITE ERYTHEMA              2 (4.2%)                            5 (15%)                           5 (11%)
      8                                                                           M              13 (39%)                           24 (55%)                          17 (50%)
      9                                            AMERICAN INDIAN OR ALASKA NATIVE               0 (NA%)                           1 (100%)                           0 (NA%)
      10                                                  BLACK OR AFRICAN AMERICAN               1 (33%)                            1 (33%)                           0 (NA%)
      11                                                                      WHITE              12 (40%)                           22 (55%)                          17 (50%)
      12                                                  APPLICATION SITE PRURITUS              1 (3.3%)                           12 (30%)                          10 (29%)
      13                                                                  DIARRHOEA               6 (20%)                           3 (7.5%)                          2 (5.9%)
      14                                                                   ERYTHEMA               3 (10%)                            5 (13%)                           6 (18%)
      15                                                  APPLICATION SITE ERYTHEMA              1 (3.3%)                           10 (25%)                           7 (21%)

# tbl_filter.tbl_hierarchical() works with various different filter conditions

    Code
      as.data.frame(tbl_f)
    Output
         **Sex**  \n    **Race**  \n        **Reported Term for the Adverse Event** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                               Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                                           F              13 (25%)                           18 (45%)                          23 (46%)
      3                                                                       WHITE              10 (21%)                           14 (41%)                          20 (45%)
      4                                                   APPLICATION SITE PRURITUS              2 (4.2%)                            8 (24%)                          10 (23%)
      5                                                                    ERYTHEMA               6 (13%)                            6 (18%)                           8 (18%)
      6                                                   APPLICATION SITE ERYTHEMA              2 (4.2%)                            5 (15%)                           5 (11%)
      7                                                                           M              13 (39%)                           24 (55%)                          17 (50%)
      8                                                                       WHITE              12 (40%)                           22 (55%)                          17 (50%)
      9                                                   APPLICATION SITE PRURITUS              1 (3.3%)                           12 (30%)                          10 (29%)
      10                                                                  DIARRHOEA               6 (20%)                           3 (7.5%)                          2 (5.9%)
      11                                                                   ERYTHEMA               3 (10%)                            5 (13%)                           6 (18%)
      12                                                  APPLICATION SITE ERYTHEMA              1 (3.3%)                           10 (25%)                           7 (21%)

# tbl_filter.tbl_hierarchical() error messaging works

    Code
      tbl_filter(data.frame(), sum(n) > 10)
    Condition
      Error in `check_class()`:
      ! The `x` argument must be class <gtsummary>, not a data frame.

---

    Code
      tbl_filter(tbl, 10)
    Condition
      Error in `tbl_filter()`:
      ! `filter` must be an expression.

