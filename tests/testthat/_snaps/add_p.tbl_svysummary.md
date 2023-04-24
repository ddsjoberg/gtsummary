# add_p creates output without error/warning

    Code
      tbl_svysummary(strial, by = grade) %>% add_p() %>% as.data.frame()
    Output
             **Characteristic**     **I**, N = 68    **II**, N = 68   **III**, N = 64
      1  Chemotherapy Treatment              <NA>              <NA>              <NA>
      2                  Drug A          35 (51%)          32 (47%)          31 (48%)
      3                  Drug B          33 (49%)          36 (53%)          33 (52%)
      4                     Age       47 (37, 56)       48 (36, 57)       47 (38, 58)
      5                 Unknown                 2                 6                 3
      6    Marker Level (ng/mL) 0.98 (0.24, 1.58) 0.37 (0.14, 1.09) 0.61 (0.26, 1.67)
      7                 Unknown                 2                 5                 3
      8                 T Stage              <NA>              <NA>              <NA>
      9                      T1          17 (25%)          23 (34%)          13 (20%)
      10                     T2          18 (26%)          17 (25%)          19 (30%)
      11                     T3          18 (26%)          11 (16%)          14 (22%)
      12                     T4          15 (22%)          17 (25%)          18 (28%)
      13         Tumor Response          21 (31%)          19 (30%)          21 (33%)
      14                Unknown                 1                 5                 1
      15           Patient Died          33 (49%)          36 (53%)          43 (67%)
      16 Months to Death/Censor 24.0 (17.8, 24.0) 21.6 (13.0, 24.0) 19.5 (15.8, 24.0)
         **p-value**
      1          0.9
      2         <NA>
      3         <NA>
      4          0.8
      5         <NA>
      6        0.016
      7         <NA>
      8          0.6
      9         <NA>
      10        <NA>
      11        <NA>
      12        <NA>
      13        >0.9
      14        <NA>
      15       0.082
      16       0.040

---

    Code
      strial %>% tbl_svysummary(by = trt) %>% add_p() %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                     Age        46 (37, 59)         48 (39, 56)         0.7
      2                 Unknown                  7                   4        <NA>
      3    Marker Level (ng/mL)  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)       0.084
      4                 Unknown                  6                   4        <NA>
      5                 T Stage               <NA>                <NA>         0.9
      6                      T1           28 (29%)            25 (25%)        <NA>
      7                      T2           25 (26%)            29 (28%)        <NA>
      8                      T3           22 (22%)            21 (21%)        <NA>
      9                      T4           23 (23%)            27 (26%)        <NA>
      10                  Grade               <NA>                <NA>         0.9
      11                      I           35 (36%)            33 (32%)        <NA>
      12                     II           32 (33%)            36 (35%)        <NA>
      13                    III           31 (32%)            33 (32%)        <NA>
      14         Tumor Response           28 (29%)            33 (34%)         0.5
      15                Unknown                  3                   4        <NA>
      16           Patient Died           52 (53%)            60 (59%)         0.4
      17 Months to Death/Censor  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)        0.14

# add_p creates output without error/warning with continuous2

    Code
      tbl_svysummary(strial, by = grade, type = all_continuous() ~ "continuous2") %>%
        add_p() %>% as.data.frame()
    Output
             **Characteristic**     **I**, N = 68    **II**, N = 68   **III**, N = 64
      1  Chemotherapy Treatment              <NA>              <NA>              <NA>
      2                  Drug A          35 (51%)          32 (47%)          31 (48%)
      3                  Drug B          33 (49%)          36 (53%)          33 (52%)
      4                     Age              <NA>              <NA>              <NA>
      5            Median (IQR)       47 (37, 56)       48 (36, 57)       47 (38, 58)
      6                 Unknown                 2                 6                 3
      7    Marker Level (ng/mL)              <NA>              <NA>              <NA>
      8            Median (IQR) 0.98 (0.24, 1.58) 0.37 (0.14, 1.09) 0.61 (0.26, 1.67)
      9                 Unknown                 2                 5                 3
      10                T Stage              <NA>              <NA>              <NA>
      11                     T1          17 (25%)          23 (34%)          13 (20%)
      12                     T2          18 (26%)          17 (25%)          19 (30%)
      13                     T3          18 (26%)          11 (16%)          14 (22%)
      14                     T4          15 (22%)          17 (25%)          18 (28%)
      15         Tumor Response          21 (31%)          19 (30%)          21 (33%)
      16                Unknown                 1                 5                 1
      17           Patient Died          33 (49%)          36 (53%)          43 (67%)
      18 Months to Death/Censor              <NA>              <NA>              <NA>
      19           Median (IQR) 24.0 (17.8, 24.0) 21.6 (13.0, 24.0) 19.5 (15.8, 24.0)
         **p-value**
      1          0.9
      2         <NA>
      3         <NA>
      4          0.8
      5         <NA>
      6         <NA>
      7        0.016
      8         <NA>
      9         <NA>
      10         0.6
      11        <NA>
      12        <NA>
      13        <NA>
      14        <NA>
      15        >0.9
      16        <NA>
      17       0.082
      18       0.040
      19        <NA>

---

    Code
      strial %>% tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2") %>%
        add_p() %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                     Age               <NA>                <NA>         0.7
      2            Median (IQR)        46 (37, 59)         48 (39, 56)        <NA>
      3                 Unknown                  7                   4        <NA>
      4    Marker Level (ng/mL)               <NA>                <NA>       0.084
      5            Median (IQR)  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)        <NA>
      6                 Unknown                  6                   4        <NA>
      7                 T Stage               <NA>                <NA>         0.9
      8                      T1           28 (29%)            25 (25%)        <NA>
      9                      T2           25 (26%)            29 (28%)        <NA>
      10                     T3           22 (22%)            21 (21%)        <NA>
      11                     T4           23 (23%)            27 (26%)        <NA>
      12                  Grade               <NA>                <NA>         0.9
      13                      I           35 (36%)            33 (32%)        <NA>
      14                     II           32 (33%)            36 (35%)        <NA>
      15                    III           31 (32%)            33 (32%)        <NA>
      16         Tumor Response           28 (29%)            33 (34%)         0.5
      17                Unknown                  3                   4        <NA>
      18           Patient Died           52 (53%)            60 (59%)         0.4
      19 Months to Death/Censor               <NA>                <NA>        0.14
      20           Median (IQR)  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)        <NA>

# add_p works well

    Code
      tbl1 %>% as.data.frame()
    Output
             **Characteristic**    **0**, N = 132     **1**, N = 61 **p-value**
      1  Chemotherapy Treatment              <NA>              <NA>         0.5
      2                  Drug A          67 (51%)          28 (46%)        <NA>
      3                  Drug B          65 (49%)          33 (54%)        <NA>
      4                     Age       46 (36, 55)       49 (42, 59)       0.090
      5                 Unknown                 7                 3        <NA>
      6    Marker Level (ng/mL) 0.59 (0.19, 1.23) 0.90 (0.29, 1.52)        0.10
      7                 Unknown                 6                 4        <NA>
      8                 T Stage              <NA>              <NA>         0.6
      9                      T1          34 (26%)          18 (30%)        <NA>
      10                     T2          39 (30%)          13 (21%)        <NA>
      11                     T3          25 (19%)          15 (25%)        <NA>
      12                     T4          34 (26%)          15 (25%)        <NA>
      13                  Grade              <NA>              <NA>        >0.9
      14                      I          46 (35%)          21 (34%)        <NA>
      15                     II          44 (33%)          19 (31%)        <NA>
      16                    III          42 (32%)          21 (34%)        <NA>
      17           Patient Died          83 (63%)          24 (39%)       0.002
      18 Months to Death/Censor 20.4 (14.8, 24.0) 24.0 (17.9, 24.0)      <0.001

---

    Code
      tbl2 %>% as.data.frame()
    Output
             **Characteristic**    **0**, N = 132     **1**, N = 61 **p-value**
      1  Chemotherapy Treatment              <NA>              <NA>         0.5
      2                  Drug A          67 (51%)          28 (46%)        <NA>
      3                  Drug B          65 (49%)          33 (54%)        <NA>
      4                     Age       46 (36, 55)       49 (42, 59)       0.081
      5                 Unknown                 7                 3        <NA>
      6    Marker Level (ng/mL) 0.59 (0.19, 1.23) 0.90 (0.29, 1.52)         0.2
      7                 Unknown                 6                 4        <NA>
      8                 T Stage              <NA>              <NA>         0.6
      9                      T1          34 (26%)          18 (30%)        <NA>
      10                     T2          39 (30%)          13 (21%)        <NA>
      11                     T3          25 (19%)          15 (25%)        <NA>
      12                     T4          34 (26%)          15 (25%)        <NA>
      13                  Grade              <NA>              <NA>        >0.9
      14                      I          46 (35%)          21 (34%)        <NA>
      15                     II          44 (33%)          19 (31%)        <NA>
      16                    III          42 (32%)          21 (34%)        <NA>
      17           Patient Died          83 (63%)          24 (39%)       0.003
      18 Months to Death/Censor 20.4 (14.8, 24.0) 24.0 (17.9, 24.0)      <0.001

