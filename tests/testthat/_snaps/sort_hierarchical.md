# sort_hierarchical() works

    Code
      as.data.frame(tbl)
    Output
         **Sex**  \n    **Race**  \n        **Reported Term for the Adverse Event** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                               Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                                           F              13 (15%)                           18 (21%)                          23 (27%)
      3                                                                       WHITE              10 (12%)                           14 (17%)                          20 (24%)
      4                                                   APPLICATION SITE PRURITUS              2 (2.3%)                           8 (9.5%)                          10 (12%)
      5                                                                    ERYTHEMA              6 (7.0%)                           6 (7.1%)                          8 (9.5%)
      6                                                   APPLICATION SITE ERYTHEMA              2 (2.3%)                           5 (6.0%)                          5 (6.0%)
      7                                                                   DIARRHOEA              2 (2.3%)                             0 (0%)                          3 (3.6%)
      8                                                   BLACK OR AFRICAN AMERICAN              3 (3.5%)                           4 (4.8%)                          3 (3.6%)
      9                                                   APPLICATION SITE PRURITUS              2 (2.3%)                           2 (2.4%)                          2 (2.4%)
      10                                                                   ERYTHEMA                0 (0%)                           1 (1.2%)                          1 (1.2%)
      11                                       ATRIOVENTRICULAR BLOCK SECOND DEGREE                0 (0%)                           1 (1.2%)                            0 (0%)
      12                                                                  DIARRHOEA              1 (1.2%)                             0 (0%)                            0 (0%)
      13                                                                          M              13 (15%)                           24 (29%)                          17 (20%)
      14                                                                      WHITE              12 (14%)                           22 (26%)                          17 (20%)
      15                                                  APPLICATION SITE PRURITUS              1 (1.2%)                           12 (14%)                          10 (12%)
      16                                                  APPLICATION SITE ERYTHEMA              1 (1.2%)                           10 (12%)                          7 (8.3%)
      17                                                                   ERYTHEMA              3 (3.5%)                           5 (6.0%)                          6 (7.1%)
      18                                                                  DIARRHOEA              6 (7.0%)                           3 (3.6%)                          2 (2.4%)
      19                                       ATRIOVENTRICULAR BLOCK SECOND DEGREE              2 (2.3%)                           2 (2.4%)                            0 (0%)
      20                                                  BLACK OR AFRICAN AMERICAN              1 (1.2%)                           1 (1.2%)                            0 (0%)
      21                                                  APPLICATION SITE PRURITUS              1 (1.2%)                             0 (0%)                            0 (0%)
      22                                                                  DIARRHOEA                0 (0%)                           1 (1.2%)                            0 (0%)
      23                                                                   ERYTHEMA                0 (0%)                           1 (1.2%)                            0 (0%)
      24                                           AMERICAN INDIAN OR ALASKA NATIVE                0 (0%)                           1 (1.2%)                            0 (0%)
      25                                                                   ERYTHEMA                0 (0%)                           1 (1.2%)                            0 (0%)

# sort_hierarchical() works when some variables not included in x

    Code
      as.data.frame(sort_hierarchical(tbl))
    Message
      As "AESOC" was not specified in `include` the event rates for variable "AESOC" were estimated by summing the rates for variable "AESEV" within each level of "AESOC". Due to unique counting of events by `id` these sums may not accurately reflect the true event rates in the data.
      As "AETERM" was not specified in `include` the event rates for variable "AETERM" were estimated by summing the rates for variable "AESEV" within each level of "AETERM". Due to unique counting of events by `id` these sums may not accurately reflect the true event rates in the data.
    Output
         **Primary System Organ Class**  \n    **Reported Term for the Adverse Event**  \n        **Severity/Intensity** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                                                                    Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                             GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                  <NA>                               <NA>                              <NA>
      3                                                                                        APPLICATION SITE PRURITUS                  <NA>                               <NA>                              <NA>
      4                                                                                                             MILD              6 (7.0%)                           17 (20%)                          19 (23%)
      5                                                                                                         MODERATE              1 (1.2%)                           12 (14%)                           9 (11%)
      6                                                                                                           SEVERE                0 (0%)                             0 (0%)                          1 (1.2%)
      7                                                                                        APPLICATION SITE ERYTHEMA                  <NA>                               <NA>                              <NA>
      8                                                                                                             MILD              3 (3.5%)                           12 (14%)                           9 (11%)
      9                                                                                                         MODERATE                0 (0%)                           6 (7.1%)                          7 (8.3%)
      10                                                                                                          SEVERE                0 (0%)                             0 (0%)                          2 (2.4%)
      11                                                                          SKIN AND SUBCUTANEOUS TISSUE DISORDERS                  <NA>                               <NA>                              <NA>
      12                                                                                                        ERYTHEMA                  <NA>                               <NA>                              <NA>
      13                                                                                                            MILD              7 (8.1%)                           12 (14%)                          8 (9.5%)
      14                                                                                                        MODERATE              4 (4.7%)                           4 (4.8%)                          8 (9.5%)
      15                                                                                      GASTROINTESTINAL DISORDERS                  <NA>                               <NA>                              <NA>
      16                                                                                                       DIARRHOEA                  <NA>                               <NA>                              <NA>
      17                                                                                                            MILD               9 (10%)                           2 (2.4%)                          5 (6.0%)
      18                                                                                                        MODERATE                0 (0%)                           2 (2.4%)                            0 (0%)
      19                                                                                               CARDIAC DISORDERS                  <NA>                               <NA>                              <NA>
      20                                                                            ATRIOVENTRICULAR BLOCK SECOND DEGREE                  <NA>                               <NA>                              <NA>
      21                                                                                                            MILD              1 (1.2%)                           1 (1.2%)                            0 (0%)
      22                                                                                                        MODERATE                0 (0%)                           2 (2.4%)                            0 (0%)
      23                                                                                                          SEVERE              1 (1.2%)                             0 (0%)                            0 (0%)

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

