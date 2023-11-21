# no errors/warnings with tidy_standardize

    Code
      tbl_regression(mod, tidy_fun = tidy_standardize) %>% as.data.frame()
    Message
      tidy_standardize(): Estimating standardized coefs with
      `parameters::standardize_parameters(model = x, ci = 0.95)`
    Output
          **Characteristic** **Beta**  **95% CI**
      1 Marker Level (ng/mL)     0.00 -0.15, 0.15
      2                Grade     <NA>        <NA>
      3                    I     <NA>        <NA>
      4                   II     0.04 -0.32, 0.41
      5                  III     0.17 -0.20, 0.53

# no errors/warnings with tidy_bootstrap

    Code
      tbl_regression(mod, tidy_fun = tidy_bootstrap) %>% as.data.frame()
    Message
      tidy_bootstrap(): Estimating bootstrapped coefs with
      `parameters::bootstrap_parameters(model = x, ci = 0.95, test = "p")`
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)    -0.09  -2.5, 2.3        >0.9
      2                Grade     <NA>       <NA>        <NA>
      3                    I     <NA>       <NA>        <NA>
      4                   II     0.67  -4.6, 6.1         0.8
      5                  III      2.5  -3.1, 7.3         0.4

# no errors/warnings with pool_and_tidy_mice

    Code
      tbl_mice %>% as.data.frame()
    Output
          **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                  Age        0.02  0.00, 0.04       0.062
      2 Marker Level (ng/mL)        0.33 -0.02, 0.68       0.065
      3                Grade        <NA>        <NA>        <NA>
      4                    I        <NA>        <NA>        <NA>
      5                   II        0.12 -0.72, 0.96         0.8
      6                  III        0.11 -0.66, 0.87         0.8

# no errors/warnings with tbl_regression.multinom

    Code
      tbl_nnet %>% as.data.frame()
    Output
        **Outcome** **Characteristic** **log(OR)** **95% CI** **p-value**
      1          II                Age         0.0   0.0, 0.0         0.6
      2         III                Age         0.0   0.0, 0.0         0.4

---

    Code
      tbl_nnet %>% as_tibble()
    Output
      # A tibble: 2 x 5
        `**Outcome**` `**Characteristic**` `**log(OR)**` `**95% CI**` `**p-value**`
        <chr>         <chr>                <chr>         <chr>        <chr>        
      1 II            Age                  0.0           0.0, 0.0     0.6          
      2 III           Age                  0.0           0.0, 0.0     0.4          

# no errors/warnings with tbl_regression.gam

    Code
      mod %>% tidy_gam()
    Output
      # A tibble: 4 x 8
        term          estimate std.error statistic p.value   edf ref.df parametric
        <chr>            <dbl>     <dbl>     <dbl>   <dbl> <dbl>  <dbl> <lgl>     
      1 (Intercept)    -0.821      0.279   -2.95   0.00320 NA     NA    TRUE      
      2 gradeII         0.0406     0.417    0.0975 0.922   NA     NA    TRUE      
      3 gradeIII       -0.0179     0.400   -0.0447 0.964   NA     NA    TRUE      
      4 s(marker,age)  NA         NA        4.63   0.0987   2.00   2.00 FALSE     

---

    Code
      mod %>% tbl_regression(exponentiate = TRUE, label = `s(marker,age)` ~
        "Smoothed marker/age") %>% as.data.frame()
    Output
         **Characteristic** **OR** **95% CI** **p-value**
      1               Grade   <NA>       <NA>        <NA>
      2                   I   <NA>       <NA>        <NA>
      3                  II   1.04 0.46, 2.36        >0.9
      4                 III   0.98 0.45, 2.15        >0.9
      5 Smoothed marker/age   <NA>       <NA>        0.10

# no errors/warnings with tidy_robust()

    Code
      glm(response ~ age + trt, trial, family = binomial) %>% tbl_regression(
        tidy_fun = purrr::partial(tidy_robust, vcov_estimation = "CL"), exponentiate = TRUE) %>%
        as.data.frame()
    Message
      Arguments `vcov` and `vcov_args` have not been specified in `tidy_robust()`. Specify at least one to obtain robust standard errors.
      tidy_robust(): Robust estimation with
      `parameters::model_parameters(model = x, ci = 0.95, vcov_estimation = "CL")`
    Output
            **Characteristic** **OR** **95% CI** **p-value**
      1                    Age   1.02 1.00, 1.04       0.095
      2 Chemotherapy Treatment   <NA>       <NA>        <NA>
      3                 Drug A   <NA>       <NA>        <NA>
      4                 Drug B   1.13 0.60, 2.13         0.7

---

    Code
      glm(response ~ age + trt, trial, family = binomial) %>% tidy_robust()
    Message
      Arguments `vcov` and `vcov_args` have not been specified in `tidy_robust()`. Specify at least one to obtain robust standard errors.
      tidy_robust(): Robust estimation with
      `parameters::model_parameters(model = x, ci = 0.95)`
    Output
               term   estimate  std.error conf.level     conf.low   conf.high
      1 (Intercept) -1.7424131 0.60136068       0.95 -2.966150321 -0.59785416
      2         age  0.0189970 0.01137703       0.95 -0.002978589  0.04182663
      3   trtDrug B  0.1254909 0.32080236       0.95 -0.502963841  0.75829003
         statistic df.error     p.value
      1 -2.8974509      Inf 0.003762086
      2  1.6697681      Inf 0.094965255
      3  0.3911783      Inf 0.695665447

