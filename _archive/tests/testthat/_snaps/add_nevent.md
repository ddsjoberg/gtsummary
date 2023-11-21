# add_nevent after tbl_regression creates output without error/warning

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Event N** **log(HR)**   **95% CI** **p-value**
      1                sex         165       -0.53 -0.86, -0.20       0.001

---

    Code
      res %>% as.data.frame()
    Output
            **Characteristic** **Event N** **log(OR)**  **95% CI** **p-value**
      1 Chemotherapy Treatment          61        <NA>        <NA>        <NA>
      2                 Drug A          NA        <NA>        <NA>        <NA>
      3                 Drug B          NA        0.19 -0.41, 0.81         0.5

# add_nevent after tbl_uvregression creates output without error/warning

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Event N** **log(HR)**  **95% CI** **p-value**
      1  Chemotherapy Treatment   200         112        <NA>        <NA>        <NA>
      2                  Drug A  <NA>          NA        <NA>        <NA>        <NA>
      3                  Drug B  <NA>          NA        0.22 -0.15, 0.59         0.2
      4                     Age   189         103        0.01 -0.01, 0.02         0.3
      5    Marker Level (ng/mL)   190         104       -0.09 -0.32, 0.14         0.4
      6                 T Stage   200         112        <NA>        <NA>        <NA>
      7                      T1  <NA>          NA        <NA>        <NA>        <NA>
      8                      T2  <NA>          NA        0.16 -0.39, 0.71         0.6
      9                      T3  <NA>          NA        0.21 -0.37, 0.79         0.5
      10                     T4  <NA>          NA        0.91   0.40, 1.4      <0.001
      11                  Grade   200         112        <NA>        <NA>        <NA>
      12                      I  <NA>          NA        <NA>        <NA>        <NA>
      13                     II  <NA>          NA        0.25 -0.23, 0.72         0.3
      14                    III  <NA>          NA        0.52  0.07, 0.98       0.024
      15         Tumor Response   193         107       -0.70 -1.2, -0.25       0.003

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Event N** **log(OR)**  **95% CI** **p-value**
      1  Chemotherapy Treatment   193          61        <NA>        <NA>        <NA>
      2                  Drug A  <NA>          NA        <NA>        <NA>        <NA>
      3                  Drug B  <NA>          NA        0.19 -0.41, 0.81         0.5
      4                     Age   183          58        0.02  0.00, 0.04        0.10
      5    Marker Level (ng/mL)   183          57        0.30 -0.06, 0.66        0.10
      6                 T Stage   193          61        <NA>        <NA>        <NA>
      7                      T1  <NA>          NA        <NA>        <NA>        <NA>
      8                      T2  <NA>          NA       -0.46  -1.3, 0.38         0.3
      9                      T3  <NA>          NA        0.13 -0.74, 0.99         0.8
      10                     T4  <NA>          NA       -0.18  -1.0, 0.65         0.7
      11                  Grade   193          61        <NA>        <NA>        <NA>
      12                      I  <NA>          NA        <NA>        <NA>        <NA>
      13                     II  <NA>          NA       -0.06 -0.81, 0.69         0.9
      14                    III  <NA>          NA        0.09 -0.65, 0.83         0.8
      15           Patient Died   193          61       -0.96 -1.6, -0.34       0.003
      16 Months to Death/Censor   193          61        0.10  0.03, 0.17       0.006

# add_nevent.tbl_surfit

    Code
      res %>% as.data.frame()
    Output
            **Characteristic** **Event N**    **Time 12**    **Time 24**
      1                Overall         112 89% (84%, 93%) 44% (38%, 51%)
      2 Chemotherapy Treatment         112           <NA>           <NA>
      3                 Drug A        <NA> 91% (85%, 97%) 47% (38%, 58%)
      4                 Drug B        <NA> 86% (80%, 93%) 41% (33%, 52%)

# add_nevent.tbl_regression

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Event N** **log(OR)**  **95% CI** **p-value**
      1              Grade          58        <NA>        <NA>        <NA>
      2                  I          NA        <NA>        <NA>        <NA>
      3                 II          NA       -0.16 -0.94, 0.61         0.7
      4                III          NA        0.01 -0.74, 0.77        >0.9
      5                Age          58        0.02  0.00, 0.04        0.10

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Event N** **log(OR)**  **95% CI** **p-value**
      1              Grade          NA        <NA>        <NA>        <NA>
      2                  I          21        <NA>        <NA>        <NA>
      3                 II          17       -0.16 -0.94, 0.61         0.7
      4                III          20        0.01 -0.74, 0.77        >0.9
      5                Age          58        0.02  0.00, 0.04        0.10

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Event N** **log(OR)**  **95% CI** **p-value**
      1              Grade          58        <NA>        <NA>        <NA>
      2                  I          21        <NA>        <NA>        <NA>
      3                 II          17       -0.16 -0.94, 0.61         0.7
      4                III          20        0.01 -0.74, 0.77        >0.9
      5                Age          58        0.02  0.00, 0.04        0.10

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **Event N** **log(OR)**  **95% CI** **p-value**
      1                Age   183          58        0.02  0.00, 0.04        0.10
      2              Grade   193          61        <NA>        <NA>        <NA>
      3                  I  <NA>          NA        <NA>        <NA>        <NA>
      4                 II  <NA>          NA       -0.06 -0.81, 0.69         0.9
      5                III  <NA>          NA        0.09 -0.65, 0.83         0.8

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **Event N** **log(OR)**  **95% CI** **p-value**
      1                Age   183          58        0.02  0.00, 0.04        0.10
      2              Grade   193          NA        <NA>        <NA>        <NA>
      3                  I  <NA>          21        <NA>        <NA>        <NA>
      4                 II  <NA>          19       -0.06 -0.81, 0.69         0.9
      5                III  <NA>          21        0.09 -0.65, 0.83         0.8

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N** **Event N** **log(OR)**  **95% CI** **p-value**
      1                Age   183          58        0.02  0.00, 0.04        0.10
      2              Grade   193          61        <NA>        <NA>        <NA>
      3                  I  <NA>          21        <NA>        <NA>        <NA>
      4                 II  <NA>          19       -0.06 -0.81, 0.69         0.9
      5                III  <NA>          21        0.09 -0.65, 0.83         0.8

