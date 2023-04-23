# no errors/warnings with standard use after tbl_regression

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1        factor(cyl)     <NA>       <NA>      <0.001
      2                  4     <NA>       <NA>        <NA>
      3                  6       15    -25, 55        <NA>
      4                  8       90    41, 139        <NA>
      5                mpg     -5.8 -9.6, -1.9       0.005
      6         factor(am)     <NA>       <NA>       0.001
      7                  0     <NA>       <NA>        <NA>
      8                  1       51     22, 79        <NA>

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1        factor(cyl)     <NA>       <NA>      <0.001
      2                  4     <NA>       <NA>        <NA>
      3                  6       15    -25, 55         0.4
      4                  8       90    41, 139      <0.001
      5                mpg     -5.8 -9.6, -1.9       0.005
      6         factor(am)     <NA>       <NA>       0.001
      7                  0     <NA>       <NA>        <NA>
      8                  1       51     22, 79       0.001

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

# `add_global_p()` works with `tbl_uvregression(x=)`

    Code
      tbl %>% as.data.frame()
    Output
                 **Outcome** **N** **Beta**   **95% CI** **p-value**
      1                  Age   189     <NA>         <NA>         0.7
      2                    I  <NA>     <NA>         <NA>        <NA>
      3                   II  <NA>      1.4    -3.6, 6.4         0.6
      4                  III  <NA>      2.0    -3.1, 7.0         0.4
      5 Marker Level (ng/mL)   190     <NA>         <NA>       0.025
      6                    I  <NA>     <NA>         <NA>        <NA>
      7                   II  <NA>    -0.39 -0.68, -0.09       0.010
      8                  III  <NA>    -0.07  -0.37, 0.23         0.6

