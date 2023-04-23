# no errors/warnings with standard use

    Code
      tbl1 %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2            Unknown                  7                   4
      3                  I           35 (36%)            33 (32%)
      4                 II           32 (33%)            36 (35%)
      5                III           31 (32%)            33 (32%)
      6                 T1           28 (29%)            25 (25%)
      7                 T2           25 (26%)            29 (28%)
      8                 T3           22 (22%)            21 (21%)
      9                 T4           23 (23%)            27 (26%)

---

    Code
      tbl3 %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1              Grade     <NA>       <NA>        <NA>
      2                 II      1.6  -3.5, 6.7         0.5
      3                III      2.2  -2.8, 7.3         0.4
      4            T Stage     <NA>       <NA>        <NA>
      5                 T2      1.4  -4.2, 7.0         0.6
      6                 T3      2.8  -3.2, 8.8         0.4
      7                 T4     -2.0  -7.9, 3.9         0.5

