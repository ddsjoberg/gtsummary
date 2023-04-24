# input checks

    Code
      tbl_summary_noby %>% modify_header(stat_0 = "test") %>% as.data.frame()
    Output
             **Characteristic**              test
      1  Chemotherapy Treatment              <NA>
      2                  Drug A          98 (49%)
      3                  Drug B         102 (51%)
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.39)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1          53 (27%)
      10                     T2          54 (27%)
      11                     T3          43 (22%)
      12                     T4          50 (25%)
      13                  Grade              <NA>
      14                      I          68 (34%)
      15                     II          68 (34%)
      16                    III          64 (32%)
      17         Tumor Response          61 (32%)
      18                Unknown                 7
      19           Patient Died         112 (56%)
      20 Months to Death/Censor 22.4 (16.0, 24.0)

---

    Code
      tbl_summary_noby %>% modify_header(stat_0 ~ "test") %>% as.data.frame()
    Output
             **Characteristic**              test
      1  Chemotherapy Treatment              <NA>
      2                  Drug A          98 (49%)
      3                  Drug B         102 (51%)
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.39)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1          53 (27%)
      10                     T2          54 (27%)
      11                     T3          43 (22%)
      12                     T4          50 (25%)
      13                  Grade              <NA>
      14                      I          68 (34%)
      15                     II          68 (34%)
      16                    III          64 (32%)
      17         Tumor Response          61 (32%)
      18                Unknown                 7
      19           Patient Died         112 (56%)
      20 Months to Death/Censor 22.4 (16.0, 24.0)

---

    Code
      tbl_summary_noby %>% modify_header(stat_0 = "N = {n}") %>% as.data.frame()
    Output
             **Characteristic**           N = 200
      1  Chemotherapy Treatment              <NA>
      2                  Drug A          98 (49%)
      3                  Drug B         102 (51%)
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.39)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1          53 (27%)
      10                     T2          54 (27%)
      11                     T3          43 (22%)
      12                     T4          50 (25%)
      13                  Grade              <NA>
      14                      I          68 (34%)
      15                     II          68 (34%)
      16                    III          64 (32%)
      17         Tumor Response          61 (32%)
      18                Unknown                 7
      19           Patient Died         112 (56%)
      20 Months to Death/Censor 22.4 (16.0, 24.0)

# checking glue inserts to headers

    Code
      tbl1 %>% as.data.frame()
    Output
             Variable (N = 200) Drug A (98/200; 49%) Drug B (102/200; 51%)
      1                     Age          46 (37, 59)           48 (39, 56)
      2                 Unknown                    7                     4
      3    Marker Level (ng/mL)    0.84 (0.24, 1.57)     0.52 (0.19, 1.20)
      4                 Unknown                    6                     4
      5                 T Stage                 <NA>                  <NA>
      6                      T1             28 (29%)              25 (25%)
      7                      T2             25 (26%)              29 (28%)
      8                      T3             22 (22%)              21 (21%)
      9                      T4             23 (23%)              27 (26%)
      10                  Grade                 <NA>                  <NA>
      11                      I             35 (36%)              33 (32%)
      12                     II             32 (33%)              36 (35%)
      13                    III             31 (32%)              33 (32%)
      14         Tumor Response             28 (29%)              33 (34%)
      15                Unknown                    3                     4
      16           Patient Died             52 (53%)              60 (59%)
      17 Months to Death/Censor    23.5 (17.4, 24.0)     21.2 (14.6, 24.0)

---

    Code
      tbl2 %>% as.data.frame()
    Output
         Variable (N = 2201: Unweighted 32)
      1                               Class
      2                                 1st
      3                                 2nd
      4                                 3rd
      5                                Crew
      6                                 Sex
      7                                Male
      8                              Female
      9                                 Age
      10                              Child
      11                              Adult
      12                               Freq
         No (1490/2201; 68%): Unweighted 16/32; 50%
      1                                        <NA>
      2                                  122 (8.2%)
      3                                   167 (11%)
      4                                   528 (35%)
      5                                   673 (45%)
      6                                        <NA>
      7                                 1,364 (92%)
      8                                  126 (8.5%)
      9                                        <NA>
      10                                  52 (3.5%)
      11                                1,438 (97%)
      12                             342 (140, 513)
         Yes (711/2201; 32%): Unweighted 16/32; 50%
      1                                        <NA>
      2                                   203 (29%)
      3                                   118 (17%)
      4                                   178 (25%)
      5                                   212 (30%)
      6                                        <NA>
      7                                   367 (52%)
      8                                   344 (48%)
      9                                        <NA>
      10                                  57 (8.0%)
      11                                  654 (92%)
      12                               79 (64, 144)

---

    Code
      tbl3 %>% as.data.frame()
    Output
        Variable (N = 32) **Beta**   **95% CI** **p-value**
      1                hp    -0.07 -0.09, -0.05      <0.001

