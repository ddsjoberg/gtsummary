# no errors/warnings with standard use after tbl_summary() and add_p()

    Code
      sort_p(table1) %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1    Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)       0.085
      2                 Unknown                  6                   4        <NA>
      3  Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)        0.14
      4            Patient Died           52 (53%)            60 (59%)         0.4
      5          Tumor Response           28 (29%)            33 (34%)         0.5
      6                 Unknown                  3                   4        <NA>
      7                     Age        46 (37, 59)         48 (39, 56)         0.7
      8                 Unknown                  7                   4        <NA>
      9                 T Stage               <NA>                <NA>         0.9
      10                     T1           28 (29%)            25 (25%)        <NA>
      11                     T2           25 (26%)            29 (28%)        <NA>
      12                     T3           22 (22%)            21 (21%)        <NA>
      13                     T4           23 (23%)            27 (26%)        <NA>
      14                  Grade               <NA>                <NA>         0.9
      15                      I           35 (36%)            33 (32%)        <NA>
      16                     II           32 (33%)            36 (35%)        <NA>
      17                    III           31 (32%)            33 (32%)        <NA>

# no errors/warnings with standard use after tbl_regression() and add_global_p()

    Code
      sort_p(regress1) %>% as.data.frame()
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1                Age   1.02 1.00, 1.04       0.092
      2              Grade   <NA>       <NA>         0.9
      3                  I   <NA>       <NA>        <NA>
      4                 II   0.85 0.39, 1.85        <NA>
      5                III   1.01 0.47, 2.16        <NA>

---

    Code
      sort_p(regress2) %>% as.data.frame()
    Output
        **Characteristic** **N** **OR** **95% CI** **p-value**
      1                Age   183   1.02 1.00, 1.04       0.091
      2              Grade   193   <NA>       <NA>        >0.9
      3                  I  <NA>   <NA>       <NA>        <NA>
      4                 II  <NA>   0.95 0.45, 2.00        <NA>
      5                III  <NA>   1.10 0.52, 2.29        <NA>

