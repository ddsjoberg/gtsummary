# add_n.tbl_regression() works

    Code
      as.data.frame(res)
    Output
            **Characteristic** **N** **OR** **95% CI** **p-value**
      1 Chemotherapy Treatment   193   <NA>       <NA>         0.5
      2                 Drug A  <NA>   <NA>       <NA>        <NA>
      3                 Drug B  <NA>   1.21 0.66, 2.24        <NA>
      4                  Grade   193   <NA>       <NA>        >0.9
      5                      I  <NA>   <NA>       <NA>        <NA>
      6                     II  <NA>   0.95 0.45, 2.00        <NA>
      7                    III  <NA>   1.10 0.52, 2.29        <NA>
      8                    Age   183   1.02 1.00, 1.04       0.091

# no errors/warnings with standard use after tbl_uvregression

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Beta**  **95% CI** **p-value**
      1  Chemotherapy Treatment   189     <NA>        <NA>         0.8
      2                  Drug A  <NA>     <NA>        <NA>        <NA>
      3                  Drug B  <NA>     0.44   -3.7, 4.6        <NA>
      4    Marker Level (ng/mL)   179    -0.05   -2.5, 2.4        >0.9
      5                 T Stage   189     <NA>        <NA>         0.5
      6                      T1  <NA>     <NA>        <NA>        <NA>
      7                      T2  <NA>      1.3   -4.2, 6.9        <NA>
      8                      T3  <NA>      2.6   -3.3, 8.6        <NA>
      9                      T4  <NA>     -2.0   -7.8, 3.8        <NA>
      10                  Grade   189     <NA>        <NA>         0.7
      11                      I  <NA>     <NA>        <NA>        <NA>
      12                     II  <NA>      1.4   -3.6, 6.4        <NA>
      13                    III  <NA>      2.0   -3.1, 7.0        <NA>
      14         Tumor Response   183      3.8  -0.66, 8.3       0.094
      15           Patient Died   189      2.2   -2.0, 6.3         0.3
      16 Months to Death/Censor   189    -0.14 -0.54, 0.26         0.5

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Beta**  **95% CI** **p-value**
      1  Chemotherapy Treatment   189     <NA>        <NA>         0.8
      2                  Drug A  <NA>     <NA>        <NA>        <NA>
      3                  Drug B  <NA>     0.44   -3.7, 4.6         0.8
      4    Marker Level (ng/mL)   179    -0.05   -2.5, 2.4        >0.9
      5                 T Stage   189     <NA>        <NA>         0.5
      6                      T1  <NA>     <NA>        <NA>        <NA>
      7                      T2  <NA>      1.3   -4.2, 6.9         0.6
      8                      T3  <NA>      2.6   -3.3, 8.6         0.4
      9                      T4  <NA>     -2.0   -7.8, 3.8         0.5
      10                  Grade   189     <NA>        <NA>         0.7
      11                      I  <NA>     <NA>        <NA>        <NA>
      12                     II  <NA>      1.4   -3.6, 6.4         0.6
      13                    III  <NA>      2.0   -3.1, 7.0         0.4
      14         Tumor Response   183      3.8  -0.66, 8.3       0.094
      15           Patient Died   189      2.2   -2.0, 6.3         0.3
      16 Months to Death/Censor   189    -0.14 -0.54, 0.26         0.5

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Beta**  **95% CI** **p-value**
      1  Chemotherapy Treatment   189     <NA>        <NA>         0.8
      2                  Drug A  <NA>     <NA>        <NA>        <NA>
      3                  Drug B  <NA>     0.44   -3.7, 4.6        <NA>
      4    Marker Level (ng/mL)   179    -0.05   -2.5, 2.4        >0.9
      5                 T Stage   189     <NA>        <NA>         0.5
      6                      T1  <NA>     <NA>        <NA>        <NA>
      7                      T2  <NA>      1.3   -4.2, 6.9        <NA>
      8                      T3  <NA>      2.6   -3.3, 8.6        <NA>
      9                      T4  <NA>     -2.0   -7.8, 3.8        <NA>
      10                  Grade   189     <NA>        <NA>         0.7
      11                      I  <NA>     <NA>        <NA>        <NA>
      12                     II  <NA>      1.4   -3.6, 6.4        <NA>
      13                    III  <NA>      2.0   -3.1, 7.0        <NA>
      14         Tumor Response   183      3.8  -0.66, 8.3       0.094
      15           Patient Died   189      2.2   -2.0, 6.3         0.3
      16 Months to Death/Censor   189    -0.14 -0.54, 0.26         0.5

---

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **N** **Beta**  **95% CI** **p-value**
      1  Chemotherapy Treatment   189     <NA>        <NA>         0.8
      2                  Drug A  <NA>     <NA>        <NA>        <NA>
      3                  Drug B  <NA>     0.44   -3.7, 4.6         0.8
      4    Marker Level (ng/mL)   179    -0.05   -2.5, 2.4        >0.9
      5                 T Stage   189     <NA>        <NA>         0.5
      6                      T1  <NA>     <NA>        <NA>        <NA>
      7                      T2  <NA>      1.3   -4.2, 6.9         0.6
      8                      T3  <NA>      2.6   -3.3, 8.6         0.4
      9                      T4  <NA>     -2.0   -7.8, 3.8         0.5
      10                  Grade   189     <NA>        <NA>         0.7
      11                      I  <NA>     <NA>        <NA>        <NA>
      12                     II  <NA>      1.4   -3.6, 6.4         0.6
      13                    III  <NA>      2.0   -3.1, 7.0         0.4
      14         Tumor Response   183      3.8  -0.66, 8.3       0.094
      15           Patient Died   189      2.2   -2.0, 6.3         0.3
      16 Months to Death/Censor   189    -0.14 -0.54, 0.26         0.5

