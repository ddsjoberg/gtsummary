# stats::glm() logistic regression works

    Code
      as.data.frame(tbl_regression(mod_logistic))
    Output
        **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                Age        0.02  0.00, 0.04       0.091
      2            T Stage        <NA>        <NA>        <NA>
      3                 T1        <NA>        <NA>        <NA>
      4                 T2       -0.54  -1.4, 0.31         0.2
      5                 T3       -0.06 -0.95, 0.82         0.9
      6                 T4       -0.23  -1.1, 0.64         0.6

---

    Code
      as.data.frame(tbl_regression(mod_logistic, exponentiate = TRUE, estimate_fun = label_style_ratio(
        digits = 1)))
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1                Age    1.0   1.0, 1.0       0.091
      2            T Stage   <NA>       <NA>        <NA>
      3                 T1   <NA>       <NA>        <NA>
      4                 T2    0.6   0.2, 1.4         0.2
      5                 T3    0.9   0.4, 2.3         0.9
      6                 T4    0.8   0.3, 1.9         0.6

# stats::glm() poisson regression works

    Code
      as.data.frame(tbl_regression(mod_poisson, show_single_row = "trt",
        estimate_fun = label_style_ratio(digits = 1)))
    Output
            **Characteristic** **log(IRR)** **95% CI** **p-value**
      1                    Age          0.0   0.0, 0.0         0.6
      2 Chemotherapy Treatment          0.0  -0.1, 0.2         0.7

---

    Code
      as.data.frame(tbl_regression(mod_poisson, exponentiate = TRUE, show_single_row = "trt",
        estimate_fun = label_style_ratio(digits = 1)))
    Output
            **Characteristic** **IRR** **95% CI** **p-value**
      1                    Age     1.0   1.0, 1.0         0.6
      2 Chemotherapy Treatment     1.0   0.9, 1.2         0.7

# stats::lm() linear regression works

    Code
      as.data.frame(tbl_regression(mod_lm))
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1                 am      -33    -83, 16         0.2

# stats::lm() works with interactions

    Code
      as.data.frame(tbl_regression(mod_lm_interaction, label = list(trt = "Tx")))
    Output
                    **Characteristic** **Beta** **95% CI** **p-value**
      1                             Tx     <NA>       <NA>        <NA>
      2                         Drug A     <NA>       <NA>        <NA>
      3                         Drug B    -0.61  -9.4, 8.2         0.9
      4                          Grade     <NA>       <NA>        <NA>
      5                              I     <NA>       <NA>        <NA>
      6                             II     0.14  -8.3, 8.6        >0.9
      7                            III      4.5   -4.9, 14         0.3
      8                 Tumor Response      4.8   -6.9, 16         0.4
      9                     Tx * Grade     <NA>       <NA>        <NA>
      10                   Drug B * II      4.2   -8.4, 17         0.5
      11                  Drug B * III     -2.9   -16, 9.9         0.7
      12           Tx * Tumor Response     <NA>       <NA>        <NA>
      13       Drug B * Tumor Response      1.3    -14, 17         0.9
      14        Grade * Tumor Response     <NA>       <NA>        <NA>
      15           II * Tumor Response     -4.4    -21, 13         0.6
      16          III * Tumor Response    -0.56    -17, 16        >0.9
      17   Tx * Grade * Tumor Response     <NA>       <NA>        <NA>
      18  Drug B * II * Tumor Response      1.3    -22, 24        >0.9
      19 Drug B * III * Tumor Response     -5.3    -28, 17         0.6

# tbl_regression(intercept)

    Code
      as.data.frame(tbl_regression(lm(age ~ marker, trial), intercept = TRUE))
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1          (Intercept)       47     44, 50      <0.001
      2 Marker Level (ng/mL)    -0.05  -2.5, 2.4        >0.9

# tbl_regression(add_estimate_to_reference_rows)

    Code
      as.data.frame(tbl_regression(lm(age ~ marker, trial),
      add_estimate_to_reference_rows = TRUE))
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)    -0.05  -2.5, 2.4        >0.9

---

    Code
      as.data.frame(tbl_regression(glm(response ~ trt, trial, family = binomial()),
      add_estimate_to_reference_rows = TRUE, exponentiate = TRUE))
    Output
            **Characteristic** **OR** **95% CI** **p-value**
      1 Chemotherapy Treatment   <NA>       <NA>        <NA>
      2                 Drug A   1.00       <NA>        <NA>
      3                 Drug B   1.21 0.66, 2.24         0.5

