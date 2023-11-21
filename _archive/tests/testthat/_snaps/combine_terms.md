# combine_terms works without error

    Code
      tbl1 %>% as.data.frame()
    Output
               **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker (non-linear terms)     <NA>       <NA>        >0.9
      2                     Stage     <NA>       <NA>        <NA>
      3                        T1     <NA>       <NA>        <NA>
      4                        T2      1.3  -4.5, 7.2         0.7
      5                        T3      2.7  -3.8, 9.1         0.4
      6                        T4     -1.8  -7.9, 4.3         0.6

---

    Code
      tbl2 %>% as.data.frame()
    Output
               **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker (non-linear terms)     <NA>       <NA>         0.8
      2                     Stage     <NA>       <NA>        <NA>
      3                        T1     <NA>       <NA>        <NA>
      4                        T2     0.96  -5.0, 6.9         0.8
      5                        T3      2.4  -4.1, 8.9         0.5
      6                        T4     -1.7  -7.8, 4.5         0.6

---

    Code
      lm(age ~ marker + I(marker^2) + stage, na.omit(trial)) %>% tbl_regression() %>%
        add_global_p() %>% combine_terms(formula = . ~ . - marker - I(marker^2)) %>%
        as.data.frame()
    Message
      combine_terms: Creating a reduced model with
      `reduced_model <- stats::update(x$model_obj, formula. = . ~ . - marker - I(marker^2))`
      combine_terms: Calculating p-value comparing full and reduced models with
      `stats::anova(x$model_obj, reduced_model)`
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)     <NA>       <NA>         0.9
      2              T Stage     <NA>       <NA>         0.5
      3                   T1     <NA>       <NA>        <NA>
      4                   T2      2.1  -3.9, 8.1        <NA>
      5                   T3      2.7  -3.9, 9.3        <NA>
      6                   T4     -1.8  -8.0, 4.4        <NA>

---

    Code
      glm(response ~ age + marker + sp2marker + sp3marker, data = trial %>% dplyr::bind_cols(
        Hmisc::rcspline.eval(.$marker, nk = 4, inclx = FALSE, norm = 0) %>%
          as.data.frame() %>% stats::setNames(c("sp2marker", "sp3marker"))) %>%
        filter(complete.cases(.) == TRUE), family = "binomial") %>% tbl_regression(
        exponentiate = TRUE) %>% combine_terms(formula_update = . ~ . - marker -
        sp2marker - sp3marker, test = "LRT") %>% as.data.frame()
    Message
      combine_terms: Creating a reduced model with
      `reduced_model <- stats::update(x$model_obj, formula. = . ~ . - marker - sp2marker - sp3marker)`
      combine_terms: Calculating p-value comparing full and reduced models with
      `stats::anova(x$model_obj, reduced_model, test = "LRT")`
    Output
          **Characteristic** **OR** **95% CI** **p-value**
      1                  Age   1.02 1.00, 1.04        0.11
      2 Marker Level (ng/mL)   <NA>       <NA>         0.5

---

    Code
      survival::coxph(survival::Surv(ttdeath, death) ~ grade + Hmisc::rcspline.eval(
        marker, nk = 4, inclx = TRUE, norm = 0), data = na.omit(trial)) %>%
        tbl_regression() %>% combine_terms(formula_update = . ~ . - Hmisc::rcspline.eval(
        marker, nk = 4, inclx = TRUE, norm = 0)) %>% as.data.frame()
    Message
      combine_terms: Creating a reduced model with
      `reduced_model <- stats::update(x$model_obj, formula. = . ~ . - Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0))`
      combine_terms: Calculating p-value comparing full and reduced models with
      `stats::anova(x$model_obj, reduced_model)`
    Output
                                                  **Characteristic** **log(HR)**
      1                                                        Grade        <NA>
      2                                                            I        <NA>
      3                                                           II        0.13
      4                                                          III        0.58
      5 Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0)        <NA>
         **95% CI** **p-value**
      1        <NA>        <NA>
      2        <NA>        <NA>
      3 -0.41, 0.67         0.6
      4   0.09, 1.1       0.021
      5        <NA>         0.7

---

    Code
      survival::survreg(survival::Surv(ttdeath, death) ~ grade + Hmisc::rcspline.eval(
        marker, nk = 4, inclx = TRUE, norm = 0), data = na.omit(trial)) %>%
        tbl_regression() %>% combine_terms(formula_update = . ~ . - Hmisc::rcspline.eval(
        marker, nk = 4, inclx = TRUE, norm = 0)) %>% as.data.frame()
    Condition
      Warning:
      The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
    Message
      combine_terms: Creating a reduced model with
      `reduced_model <- stats::update(x$model_obj, formula. = . ~ . - Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0))`
      combine_terms: Calculating p-value comparing full and reduced models with
      `stats::anova(x$model_obj, reduced_model)`
    Condition
      Warning:
      The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
    Output
                                                  **Characteristic** **Beta**
      1                                                        Grade     <NA>
      2                                                            I     <NA>
      3                                                           II    -0.05
      4                                                          III    -0.21
      5 Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0)     <NA>
          **95% CI** **p-value**
      1         <NA>        <NA>
      2         <NA>        <NA>
      3  -0.24, 0.15         0.6
      4 -0.39, -0.03       0.021
      5         <NA>         0.7

---

    Code
      geepack::geeglm(as.formula("weight ~ Diet + Time + sp2Time + sp3Time"), data = ChickWeight %>%
        dplyr::bind_cols(Hmisc::rcspline.eval(.$Time, nk = 4, inclx = FALSE, norm = 0) %>%
          as.data.frame() %>% stats::setNames(c("sp2Time", "sp3Time"))), family = gaussian,
      id = Chick, corstr = "exchangeable") %>% tbl_regression() %>% combine_terms(
        formula_update = . ~ . - Time - sp2Time - sp3Time) %>% as.data.frame()
    Message
      combine_terms: Creating a reduced model with
      `reduced_model <- stats::update(x$model_obj, formula. = . ~ . - Time - sp2Time - sp3Time)`
      combine_terms: Calculating p-value comparing full and reduced models with
      `stats::anova(x$model_obj, reduced_model)`
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1               Diet     <NA>       <NA>        <NA>
      2                  1     <NA>       <NA>        <NA>
      3                  2       16   -4.5, 37        0.13
      4                  3       37     18, 55      <0.001
      5                  4       30     18, 43      <0.001
      6               Time     <NA>       <NA>      <0.001

