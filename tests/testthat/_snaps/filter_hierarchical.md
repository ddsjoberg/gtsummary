# filter_hierarchical() works

    Code
      as.data.frame(tbl)
    Output
         **Sex**  \n    **Race**  \n        **Reported Term for the Adverse Event** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                               Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                                           F              13 (15%)                           18 (21%)                          23 (27%)
      3                                                                       WHITE              10 (12%)                           14 (17%)                          20 (24%)
      4                                                   APPLICATION SITE ERYTHEMA              2 (2.3%)                           5 (6.0%)                          5 (6.0%)
      5                                                   APPLICATION SITE PRURITUS              2 (2.3%)                           8 (9.5%)                          10 (12%)
      6                                                                    ERYTHEMA              6 (7.0%)                           6 (7.1%)                          8 (9.5%)
      7                                                                           M              13 (15%)                           24 (29%)                          17 (20%)
      8                                                                       WHITE              12 (14%)                           22 (26%)                          17 (20%)
      9                                                   APPLICATION SITE ERYTHEMA              1 (1.2%)                           10 (12%)                          7 (8.3%)
      10                                                  APPLICATION SITE PRURITUS              1 (1.2%)                           12 (14%)                          10 (12%)
      11                                                                  DIARRHOEA              6 (7.0%)                           3 (3.6%)                          2 (2.4%)
      12                                                                   ERYTHEMA              3 (3.5%)                           5 (6.0%)                          6 (7.1%)

# filter_hierarchical(var) works

    Code
      tbl_f$table_body$label
    Output
       [1] "GASTROINTESTINAL DISORDERS"                           "DIARRHOEA"                                            "MILD"                                                
       [4] "MODERATE"                                             "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "APPLICATION SITE ERYTHEMA"                           
       [7] "MILD"                                                 "MODERATE"                                             "SEVERE"                                              
      [10] "APPLICATION SITE PRURITUS"                            "MILD"                                                 "MODERATE"                                            
      [13] "SEVERE"                                               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               "ERYTHEMA"                                            
      [16] "MILD"                                                 "MODERATE"                                            

# filter_hierarchical() works with various different filter conditions

    Code
      as.data.frame(tbl_f)
    Output
         **Sex**  \n    **Race**  \n        **Reported Term for the Adverse Event** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                               Number of patients with event              26 (30%)                           42 (50%)                          40 (48%)
      2                                                                           F              13 (15%)                           18 (21%)                          23 (27%)
      3                                                                       WHITE              10 (12%)                           14 (17%)                          20 (24%)
      4                                                   APPLICATION SITE ERYTHEMA              2 (2.3%)                           5 (6.0%)                          5 (6.0%)
      5                                                   APPLICATION SITE PRURITUS              2 (2.3%)                           8 (9.5%)                          10 (12%)
      6                                                                    ERYTHEMA              6 (7.0%)                           6 (7.1%)                          8 (9.5%)
      7                                                                           M              13 (15%)                           24 (29%)                          17 (20%)
      8                                                                       WHITE              12 (14%)                           22 (26%)                          17 (20%)
      9                                                   APPLICATION SITE ERYTHEMA              1 (1.2%)                           10 (12%)                          7 (8.3%)
      10                                                  APPLICATION SITE PRURITUS              1 (1.2%)                           12 (14%)                          10 (12%)
      11                                                                  DIARRHOEA              6 (7.0%)                           3 (3.6%)                          2 (2.4%)
      12                                                                   ERYTHEMA              3 (3.5%)                           5 (6.0%)                          6 (7.1%)

# filter_hierarchical() error messaging works

    Code
      filter_hierarchical(data.frame(), sum(n) > 10)
    Condition
      Error in `filter_hierarchical()`:
      ! The `x` argument must be class <gtsummary>, not a data frame.

---

    Code
      filter_hierarchical(tbl, 10)
    Condition
      Error in `filter_hierarchical()`:
      ! The `filter` argument must be an expression.

