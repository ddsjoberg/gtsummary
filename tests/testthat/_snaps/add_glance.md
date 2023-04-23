# add_glance_source_note: no errors/warnings with standard use

    Code
      res %>% as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)    -0.04  -2.6, 2.5       0.976
      2                Grade     <NA>       <NA>        <NA>
      3                    I     <NA>       <NA>        <NA>
      4                   II     0.64  -4.7, 6.0       0.814
      5                  III      2.4  -2.8, 7.6       0.366

---

    Code
      tbl_glance %>% as.data.frame()
    Output
           **Characteristic** **Beta** **95% CI** **p-value**
      1  Marker Level (ng/mL)    -0.04  -2.6, 2.5       0.976
      2                 Grade     <NA>       <NA>        <NA>
      3                     I     <NA>       <NA>        <NA>
      4                    II     0.64  -4.7, 6.0       0.814
      5                   III      2.4  -2.8, 7.6       0.366
      6                    R²    0.005       <NA>        <NA>
      7                   AIC    1,473       <NA>        <NA>
      8                     σ     14.6       <NA>        <NA>
      9    Degrees of Freedom        3       <NA>        <NA>
      10             No. Obs.      179       <NA>        <NA>
      11              p-value    0.832       <NA>        <NA>

---

    Code
      res %>% as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI**
      1 Marker Level (ng/mL)    -0.05  -2.5, 2.4

---

    Code
      res %>% as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)    -0.04  -2.6, 2.5       0.976
      2                Grade     <NA>       <NA>        <NA>
      3                    I     <NA>       <NA>        <NA>
      4                   II     0.64  -4.7, 6.0       0.814
      5                  III      2.4  -2.8, 7.6       0.366
      6                   R²    0.005       <NA>        <NA>
      7                  AIC    1,473       <NA>        <NA>
      8                    σ     14.6       <NA>        <NA>
      9   Degrees of Freedom        3       <NA>        <NA>

---

    Code
      res %>% as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI**
      1 Marker Level (ng/mL)    -0.05  -2.5, 2.4
      2             No. Obs.      179       <NA>
      3                Sigma     14.5       <NA>
      4       Log-likelihood     -730       <NA>
      5                  AIC    1,467       <NA>
      6                  BIC    1,480       <NA>
      7             REMLcrit    1,459       <NA>
      8          Residual df      175       <NA>

