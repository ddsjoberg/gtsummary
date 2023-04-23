# lm: no errors/warnings with standard use

    Code
      mtcars %>% tbl_uvregression(method = lm, y = mpg) %>% as.data.frame()
    Output
         **Characteristic** **N** **Beta**   **95% CI** **p-value**
      1                 cyl    32     -2.9   -3.5, -2.2      <0.001
      2                disp    32    -0.04 -0.05, -0.03      <0.001
      3                  hp    32    -0.07 -0.09, -0.05      <0.001
      4                drat    32      7.7      4.6, 11      <0.001
      5                  wt    32     -5.3   -6.5, -4.2      <0.001
      6                qsec    32      1.4    0.27, 2.6       0.017
      7                  vs    32      7.9      4.6, 11      <0.001
      8                  am    32      7.2      3.6, 11      <0.001
      9                gear    32      3.9     1.3, 6.6       0.005
      10               carb    32     -2.1  -3.2, -0.89       0.001

---

    Code
      mtcars %>% tbl_uvregression(method = lm, y = "mpg") %>% as.data.frame()
    Output
         **Characteristic** **N** **Beta**   **95% CI** **p-value**
      1                 cyl    32     -2.9   -3.5, -2.2      <0.001
      2                disp    32    -0.04 -0.05, -0.03      <0.001
      3                  hp    32    -0.07 -0.09, -0.05      <0.001
      4                drat    32      7.7      4.6, 11      <0.001
      5                  wt    32     -5.3   -6.5, -4.2      <0.001
      6                qsec    32      1.4    0.27, 2.6       0.017
      7                  vs    32      7.9      4.6, 11      <0.001
      8                  am    32      7.2      3.6, 11      <0.001
      9                gear    32      3.9     1.3, 6.6       0.005
      10               carb    32     -2.1  -3.2, -0.89       0.001

# geeglm: no errors/warnings with standard use

    Code
      tbl_uvregression(na.omit(trial), y = age, method = geepack::geeglm,
      method.args = list(id = response, corstr = "exchangeable"), include = -response) %>%
        as.data.frame()
    Output
             **Characteristic** **N** **Beta**  **95% CI** **p-value**
      1  Chemotherapy Treatment   173     <NA>        <NA>        <NA>
      2                  Drug A  <NA>     <NA>        <NA>        <NA>
      3                  Drug B  <NA>     0.51   -3.7, 4.7         0.8
      4    Marker Level (ng/mL)   173     0.28   -2.1, 2.6         0.8
      5                 T Stage   173     <NA>        <NA>        <NA>
      6                      T1  <NA>     <NA>        <NA>        <NA>
      7                      T2  <NA>      2.0   -3.9, 7.8         0.5
      8                      T3  <NA>      3.0   -2.7, 8.7         0.3
      9                      T4  <NA>     -1.7   -7.7, 4.3         0.6
      10                  Grade   173     <NA>        <NA>        <NA>
      11                      I  <NA>     <NA>        <NA>        <NA>
      12                     II  <NA>     0.16   -5.5, 5.8        >0.9
      13                    III  <NA>      2.3   -3.1, 7.8         0.4
      14           Patient Died   173      2.3   -1.9, 6.5         0.3
      15 Months to Death/Censor   173    -0.21 -0.61, 0.18         0.3

# lm specifying tidy_fun: no errors/warnings with standard use

    Code
      mtcars %>% tbl_uvregression(method = lm, y = mpg, tidy_fun = broom::tidy) %>%
        as.data.frame()
    Output
         **Characteristic** **N** **Beta**   **95% CI** **p-value**
      1                 cyl    32     -2.9   -3.5, -2.2      <0.001
      2                disp    32    -0.04 -0.05, -0.03      <0.001
      3                  hp    32    -0.07 -0.09, -0.05      <0.001
      4                drat    32      7.7      4.6, 11      <0.001
      5                  wt    32     -5.3   -6.5, -4.2      <0.001
      6                qsec    32      1.4    0.27, 2.6       0.017
      7                  vs    32      7.9      4.6, 11      <0.001
      8                  am    32      7.2      3.6, 11      <0.001
      9                gear    32      3.9     1.3, 6.6       0.005
      10               carb    32     -2.1  -3.2, -0.89       0.001

# coxph: no errors/warnings with standard use

    Code
      coxph_uv %>% as.data.frame()
    Output
        **Characteristic** **N** **log(HR)**   **95% CI** **p-value**
      1               inst   227       -0.01  -0.03, 0.01         0.3
      2                age   228        0.02   0.00, 0.04       0.042
      3                sex   228       -0.53 -0.86, -0.20       0.001
      4            ph.ecog   227        0.48   0.25, 0.70      <0.001
      5           ph.karno   227       -0.02  -0.03, 0.00       0.005
      6          pat.karno   225       -0.02 -0.03, -0.01      <0.001
      7           meal.cal   181        0.00   0.00, 0.00         0.6
      8            wt.loss   214        0.00  -0.01, 0.01         0.8

# glmer: no errors/warnings with standard use

    Code
      lme4_uv %>% as.data.frame()
    Output
        **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                 hp       -0.01 -0.05, 0.03         0.5
      2      No. Cylinders       -0.51  -1.4, 0.39         0.3

---

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                 hp       -0.01 -0.05, 0.03         0.5
      2      No. Cylinders       -0.51  -1.4, 0.39         0.3

# tbl_uvregression x= argument tests

    Code
      ux_x %>% as.data.frame()
    Output
                 **Outcome** **N** **Beta**  **95% CI** **p-value**
      1          PATIENT AGE   183      3.8  -0.66, 8.3       0.094
      2 Marker Level (ng/mL)   183     0.23 -0.04, 0.50        0.10

# tbl_uvregression does not throw error with odd variable names in `data=`

    Code
      tbl %>% as.data.frame()
    Output
             **Characteristic** **N** **Beta**  **95% CI** **p-value**
      1  Chemotherapy Treatment   189     <NA>        <NA>        <NA>
      2                  Drug A  <NA>     <NA>        <NA>        <NA>
      3                  Drug B  <NA>     0.44   -3.7, 4.6         0.8
      4    Marker Level (ng/mL)   179    -0.05   -2.5, 2.4        >0.9
      5                 T Stage   189     <NA>        <NA>        <NA>
      6                      T1  <NA>     <NA>        <NA>        <NA>
      7                      T2  <NA>      1.3   -4.2, 6.9         0.6
      8                      T3  <NA>      2.6   -3.3, 8.6         0.4
      9                      T4  <NA>     -2.0   -7.8, 3.8         0.5
      10                  Grade   189     <NA>        <NA>        <NA>
      11                      I  <NA>     <NA>        <NA>        <NA>
      12                     II  <NA>      1.4   -3.6, 6.4         0.6
      13                    III  <NA>      2.0   -3.1, 7.0         0.4
      14         Tumor Response   183      3.8  -0.66, 8.3       0.094
      15           Patient Died   189      2.2   -2.0, 6.3         0.3
      16 Months to Death/Censor   189    -0.14 -0.54, 0.26         0.5

# tbl_uvregression works with survey object

    Code
      tbl_uvreg %>% as.data.frame()
    Output
          **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                  Age        0.02  0.00, 0.04        0.10
      2 Marker Level (ng/mL)        0.30 -0.06, 0.65        0.10
      3                Grade        <NA>        <NA>        <NA>
      4                    I        <NA>        <NA>        <NA>
      5                   II       -0.06 -0.81, 0.70         0.9
      6                  III        0.09 -0.65, 0.83         0.8

