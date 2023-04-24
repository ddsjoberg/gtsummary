# test-select helpers

    Code
      tbl_summary(trial, statistic = all_continuous() ~ "{mean}") %>% as.data.frame()
    Output
             **Characteristic** **N = 200**
      1  Chemotherapy Treatment        <NA>
      2                  Drug A    98 (49%)
      3                  Drug B   102 (51%)
      4                     Age          47
      5                 Unknown          11
      6    Marker Level (ng/mL)        0.92
      7                 Unknown          10
      8                 T Stage        <NA>
      9                      T1    53 (27%)
      10                     T2    54 (27%)
      11                     T3    43 (22%)
      12                     T4    50 (25%)
      13                  Grade        <NA>
      14                      I    68 (34%)
      15                     II    68 (34%)
      16                    III    64 (32%)
      17         Tumor Response    61 (32%)
      18                Unknown           7
      19           Patient Died   112 (56%)
      20 Months to Death/Censor        19.6

---

    Code
      tbl_summary(trial, statistic = all_categorical() ~ "{n}") %>% as.data.frame()
    Output
             **Characteristic**       **N = 200**
      1  Chemotherapy Treatment              <NA>
      2                  Drug A                98
      3                  Drug B               102
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.39)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1                53
      10                     T2                54
      11                     T3                43
      12                     T4                50
      13                  Grade              <NA>
      14                      I                68
      15                     II                68
      16                    III                64
      17         Tumor Response                61
      18                Unknown                 7
      19           Patient Died               112
      20 Months to Death/Censor 22.4 (16.0, 24.0)

---

    Code
      tbl_summary(trial, statistic = all_dichotomous() ~ "{n}") %>% as.data.frame()
    Output
             **Characteristic**       **N = 200**
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
      17         Tumor Response                61
      18                Unknown                 7
      19           Patient Died               112
      20 Months to Death/Censor 22.4 (16.0, 24.0)

---

    Code
      tbl_summary(trial, statistic = all_categorical(dichotomous = FALSE) ~ "{n}") %>%
        as.data.frame()
    Output
             **Characteristic**       **N = 200**
      1  Chemotherapy Treatment              <NA>
      2                  Drug A                98
      3                  Drug B               102
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.39)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1                53
      10                     T2                54
      11                     T3                43
      12                     T4                50
      13                  Grade              <NA>
      14                      I                68
      15                     II                68
      16                    III                64
      17         Tumor Response          61 (32%)
      18                Unknown                 7
      19           Patient Died         112 (56%)
      20 Months to Death/Censor 22.4 (16.0, 24.0)

---

    Code
      tbl_summary(trial, by = trt, include = c(stage, response, trt)) %>% add_p(test = everything() ~
        "fisher.test", test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
        as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1            T Stage               <NA>                <NA>         0.9
      2                 T1           28 (29%)            25 (25%)        <NA>
      3                 T2           25 (26%)            29 (28%)        <NA>
      4                 T3           22 (22%)            21 (21%)        <NA>
      5                 T4           23 (23%)            27 (26%)        <NA>
      6     Tumor Response           28 (29%)            33 (34%)         0.5
      7            Unknown                  3                   4        <NA>

