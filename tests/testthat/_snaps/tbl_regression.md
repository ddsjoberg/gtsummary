# glm: logistic and poisson regression

    Code
      tbl_regression(mod_logistic) %>% as.data.frame()
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
      tbl_regression(mod_poisson, show_single_row = "trt", estimate_fun = purrr::partial(
        style_ratio, digits = 1)) %>% as_tibble()
    Output
      # A tibble: 2 x 4
        `**Characteristic**`   `**log(IRR)**` `**95% CI**` `**p-value**`
        <chr>                  <chr>          <chr>        <chr>        
      1 Age                    0.0            0.0, 0.0     0.6          
      2 Chemotherapy Treatment 0.0            -0.1, 0.2    0.7          

---

    Code
      tbl_regression(mod_logistic, exponentiate = TRUE, estimate_fun = purrr::partial(
        style_ratio, digits = 1)) %>% as.data.frame()
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1                Age    1.0   1.0, 1.0       0.091
      2            T Stage   <NA>       <NA>        <NA>
      3                 T1   <NA>       <NA>        <NA>
      4                 T2    0.6   0.2, 1.4         0.2
      5                 T3    0.9   0.4, 2.3         0.9
      6                 T4    0.8   0.3, 1.9         0.6

---

    Code
      tbl_regression(mod_poisson, exponentiate = TRUE, show_single_row = "trt",
        estimate_fun = purrr::partial(style_ratio, digits = 1)) %>% as_tibble()
    Output
      # A tibble: 2 x 4
        `**Characteristic**`   `**IRR**` `**95% CI**` `**p-value**`
        <chr>                  <chr>     <chr>        <chr>        
      1 Age                    1.0       1.0, 1.0     0.6          
      2 Chemotherapy Treatment 1.0       0.9, 1.2     0.7          

# lm: no errors/warnings with standard use

    Code
      tbl_regression(mod_lm) %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1                 am      -33    -83, 16         0.2

# lm with tidyfun: no errors/warnings with standard use

    Code
      tbl_regression(mod_lm, tidy_fun = broom::tidy) %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1                 am      -33    -83, 16         0.2

# survreg: no errors/warnings with standard use

    Code
      tbl_regression(mod_survreg) %>% as.data.frame()
    Condition
      Warning:
      The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
    Output
        **Characteristic** **Beta**   **95% CI** **p-value**
      1                age    -0.01  -0.02, 0.01         0.3
      2            ph.ecog    -0.33 -0.49, -0.16      <0.001

# lmer: no errors/warnings with standard use

    Code
      tbl_regression(mod_lmer) %>% as.data.frame()
    Output
        **Characteristic** **Beta** **95% CI**
      1               Days       10    7.4, 13

# glmer: no errors/warnings with standard use

    Code
      tbl_regression(mod_glmer) %>% as.data.frame()
    Output
        **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                 hp        0.00 -0.07, 0.07        >0.9
      2        factor(cyl)        <NA>        <NA>        <NA>
      3                  4        <NA>        <NA>        <NA>
      4                  6        -1.3   -5.1, 2.5         0.5
      5                  8        -2.1    -14, 9.7         0.7

# lm with interactions: no errors/warnings with standard use

    Code
      tbl_regression(mod_lm_interaction) %>% as.data.frame()
    Output
                                      **Characteristic** **Beta** **95% CI**
      1                           Chemotherapy Treatment     <NA>       <NA>
      2                                           Drug A     <NA>       <NA>
      3                                           Drug B    -0.61  -9.4, 8.2
      4                                            Grade     <NA>       <NA>
      5                                                I     <NA>       <NA>
      6                                               II     0.14  -8.3, 8.6
      7                                              III      4.5   -4.9, 14
      8                                   Tumor Response      4.8   -6.9, 16
      9                   Chemotherapy Treatment * Grade     <NA>       <NA>
      10                                     Drug B * II      4.2   -8.4, 17
      11                                    Drug B * III     -2.9   -16, 9.9
      12         Chemotherapy Treatment * Tumor Response     <NA>       <NA>
      13                         Drug B * Tumor Response      1.3    -14, 17
      14                          Grade * Tumor Response     <NA>       <NA>
      15                             II * Tumor Response     -4.4    -21, 13
      16                            III * Tumor Response    -0.56    -17, 16
      17 Chemotherapy Treatment * Grade * Tumor Response     <NA>       <NA>
      18                    Drug B * II * Tumor Response      1.3    -22, 24
      19                   Drug B * III * Tumor Response     -5.3    -28, 17
         **p-value**
      1         <NA>
      2         <NA>
      3          0.9
      4         <NA>
      5         <NA>
      6         >0.9
      7          0.3
      8          0.4
      9         <NA>
      10         0.5
      11         0.7
      12        <NA>
      13         0.9
      14        <NA>
      15         0.6
      16        >0.9
      17        <NA>
      18        >0.9
      19         0.6

# Testing lme4 results

    Code
      tbl_lme4 %>% as.data.frame()
    Output
        **Characteristic** **OR** **90% CI** **p-value**
      1                 hp   0.97 0.93, 1.00        0.15
      2         factor(vs)   <NA>       <NA>        <NA>
      3                  0   <NA>       <NA>        <NA>
      4                  1   0.01 0.00, 12.4         0.3

# Interaction modifications

    Code
      tbl_i %>% as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1     factor(response)     <NA>       <NA>        <NA>
      2                    0     <NA>       <NA>        <NA>
      3                    1      9.1    2.1, 16       0.011
      4 Marker Level (ng/mL)      2.0  -1.2, 5.2         0.2
      5          Interaction     -5.3 -11, -0.11       0.045

# tidycrr models work

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic** **HR** **95% CI** **p-value**
      1                Age   1.01 0.99, 1.03         0.6
      2              Grade   <NA>       <NA>        <NA>
      3                  I   <NA>       <NA>        <NA>
      4                 II   1.06 0.52, 2.16         0.9
      5                III   1.54 0.82, 2.90         0.2

---

    Code
      add_global_p(tbl) %>% as.data.frame()
    Output
        **Characteristic** **HR** **95% CI** **p-value**
      1                Age   1.01 0.99, 1.03         0.6
      2              Grade   <NA>       <NA>         0.3
      3                  I   <NA>       <NA>        <NA>
      4                 II   1.06 0.52, 2.16        <NA>
      5                III   1.54 0.82, 2.90        <NA>

# NSE args could be passed to tidy_plus_plus()

    Code
      res %>% as.data.frame()
    Output
             **Characteristic** **OR** **95% CI** **p-value**
      1                     Age   1.02 1.00, 1.04       0.092
      2  Chemotherapy Treatment   <NA>       <NA>        <NA>
      3                  Drug A   <NA>       <NA>        <NA>
      4                  Drug B   1.15 0.61, 2.18         0.7
      5                 T Stage   <NA>       <NA>        <NA>
      6                      T2   0.56 0.23, 1.33         0.2
      7                      T3   0.90 0.37, 2.21         0.8
      8                      T4   0.76 0.31, 1.85         0.6
      9                   Grade   <NA>       <NA>        <NA>
      10                     II   0.84 0.38, 1.85         0.7
      11                    III   1.04 0.48, 2.23        >0.9

# `add_header_rows = FALSE` could be passed to tidy_plus_plus()

    Code
      res %>% as.data.frame()
    Output
         **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                 Age        0.02  0.00, 0.04       0.092
      2              Drug A        <NA>        <NA>        <NA>
      3              Drug B        0.14 -0.49, 0.78         0.7
      4                  T1        <NA>        <NA>        <NA>
      5                  T2       -0.57  -1.5, 0.29         0.2
      6                  T3       -0.10  -1.0, 0.79         0.8
      7                  T4       -0.27  -1.2, 0.62         0.6
      8                   I        <NA>        <NA>        <NA>
      9                  II       -0.17 -0.97, 0.61         0.7
      10                III        0.04 -0.73, 0.80        >0.9

