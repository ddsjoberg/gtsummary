# sort_hierarchical() works

    Code
      as.data.frame(tbl)
    Output
         **Sex**  \n    **Race**  \n        **Reported Term for the Adverse Event** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                               Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                                           F              13 (25%)                           18 (45%)                          23 (46%)
      3                                                                       WHITE              10 (21%)                           14 (41%)                          20 (45%)
      4                                                   APPLICATION SITE PRURITUS              2 (4.2%)                            8 (24%)                          10 (23%)
      5                                                                    ERYTHEMA               6 (13%)                            6 (18%)                           8 (18%)
      6                                                   APPLICATION SITE ERYTHEMA              2 (4.2%)                            5 (15%)                           5 (11%)
      7                                                                   DIARRHOEA              2 (4.2%)                             0 (0%)                          3 (6.8%)
      8                                                   BLACK OR AFRICAN AMERICAN               3 (60%)                            4 (67%)                           3 (50%)
      9                                                   APPLICATION SITE PRURITUS               2 (40%)                            2 (33%)                           2 (33%)
      10                                                                   ERYTHEMA                0 (0%)                            1 (17%)                           1 (17%)
      11                                       ATRIOVENTRICULAR BLOCK SECOND DEGREE                0 (0%)                            1 (17%)                            0 (0%)
      12                                                                  DIARRHOEA               1 (20%)                             0 (0%)                            0 (0%)
      13                                                                          M              13 (39%)                           24 (55%)                          17 (50%)
      14                                                                      WHITE              12 (40%)                           22 (55%)                          17 (50%)
      15                                                  APPLICATION SITE PRURITUS              1 (3.3%)                           12 (30%)                          10 (29%)
      16                                                  APPLICATION SITE ERYTHEMA              1 (3.3%)                           10 (25%)                           7 (21%)
      17                                                                   ERYTHEMA               3 (10%)                            5 (13%)                           6 (18%)
      18                                                                  DIARRHOEA               6 (20%)                           3 (7.5%)                          2 (5.9%)
      19                                       ATRIOVENTRICULAR BLOCK SECOND DEGREE              2 (6.7%)                           2 (5.0%)                            0 (0%)
      20                                                  BLACK OR AFRICAN AMERICAN               1 (33%)                            1 (33%)                           0 (NA%)
      21                                                  APPLICATION SITE PRURITUS               1 (33%)                             0 (0%)                           0 (NA%)
      22                                                                  DIARRHOEA                0 (0%)                            1 (33%)                           0 (NA%)
      23                                                                   ERYTHEMA                0 (0%)                            1 (33%)                           0 (NA%)
      24                                           AMERICAN INDIAN OR ALASKA NATIVE               0 (NA%)                           1 (100%)                           0 (NA%)
      25                                                                   ERYTHEMA               0 (NA%)                           1 (100%)                           0 (NA%)

# sort_hierarchical() error messaging works

    Code
      sort_hierarchical(data.frame())
    Condition
      Error in `sort_hierarchical()`:
      ! The `x` argument must be class <gtsummary>, not a data frame.

---

    Code
      sort_hierarchical(tbl, "10")
    Condition
      Error in `sort_hierarchical()`:
      ! Sorting type must be either "descending" or "alphanumeric" for all variables.

