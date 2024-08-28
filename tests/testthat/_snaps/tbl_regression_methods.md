# tbl_regression.model_fit()

    Code
      as.data.frame(tbl_regression(parsnip::fit(parsnip::set_mode(parsnip::set_engine(
        parsnip::linear_reg(), "lm"), "regression"), age ~ grade + stage, data = trial)))
    Message
      Extracting {parsnip} model fit with `tbl_regression(x = x$fit, ...)`
    Output
        **Characteristic** **Beta** **95% CI** **p-value**
      1              Grade     <NA>       <NA>        <NA>
      2                  I     <NA>       <NA>        <NA>
      3                 II      1.6  -3.5, 6.7         0.5
      4                III      2.2  -2.8, 7.3         0.4
      5            T Stage     <NA>       <NA>        <NA>
      6                 T1     <NA>       <NA>        <NA>
      7                 T2      1.4  -4.2, 7.0         0.6
      8                 T3      2.8  -3.2, 8.8         0.4
      9                 T4     -2.0  -7.9, 3.9         0.5

# tbl_regression.workflow()

    Code
      as.data.frame(tbl_regression(parsnip::fit(workflows::add_formula(workflows::add_model(
        workflows::workflow(), parsnip::set_engine(parsnip::logistic_reg(), "glm")),
      factor(response) ~ age + stage), data = trial)))
    Message
      i To take full advantage of model formatting, e.g. grouping categorical variables, please add the following argument to the `workflows::add_model()` call:
      * `blueprint = hardhat::default_formula_blueprint(indicators = 'none')`
      Extracting {parsnip} model fit with `tbl_regression(x = x$fit, ...)`
    Output
        **Characteristic** **log(OR)**  **95% CI** **p-value**
      1                age        0.02  0.00, 0.04       0.091
      2            stageT2       -0.54  -1.4, 0.31         0.2
      3            stageT3       -0.06 -0.95, 0.82         0.9
      4            stageT4       -0.23  -1.1, 0.64         0.6

# tbl_regression.survreg()

    Code
      as.data.frame(tbl_regression(survival::survreg(survival::Surv(time, status) ~
        age + ph.ecog, data = survival::lung)))
    Condition
      Warning:
      The `exponentiate` argument is not supported in the `tidy()` method for `survreg` objects and will be ignored.
    Output
        **Characteristic** **Beta**   **95% CI** **p-value**
      1                age    -0.01  -0.02, 0.01         0.3
      2            ph.ecog    -0.33 -0.49, -0.16      <0.001

# tbl_regression.mira()

    Code
      as.data.frame(tbl_regression(with(suppressWarnings(mice::mice(trial, m = 2)),
      lm(age ~ marker + grade))))
    Output
      
       iter imp variable
        1   1  age  marker  response
        1   2  age  marker  response
        2   1  age  marker  response
        2   2  age  marker  response
        3   1  age  marker  response
        3   2  age  marker  response
        4   1  age  marker  response
        4   2  age  marker  response
        5   1  age  marker  response
        5   2  age  marker  response
          **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)     0.24  -2.2, 2.6         0.8
      2                Grade     <NA>       <NA>        <NA>
      3                    I     <NA>       <NA>        <NA>
      4                   II      1.3  -4.5, 7.0         0.7
      5                  III      1.9  -3.3, 7.1         0.5

---

    Code
      as.data.frame(tbl_regression(mice::pool(with(suppressWarnings(mice::mice(trial,
        m = 2)), lm(age ~ marker + grade)))))
    Output
      
       iter imp variable
        1   1  age  marker  response
        1   2  age  marker  response
        2   1  age  marker  response
        2   2  age  marker  response
        3   1  age  marker  response
        3   2  age  marker  response
        4   1  age  marker  response
        4   2  age  marker  response
        5   1  age  marker  response
        5   2  age  marker  response
    Message
      i Pass the <mice> model to `tbl_regression()` before models have been combined with `mice::pool()`.
      * The default tidier, `pool_and_tidy_mice()`, will both pool and tidy the regression model.
      * `mice::mice(trial, m = 2) |> with(lm(age ~ marker + grade)) |> tbl_regression()`
    Output
      data frame with 0 columns and 0 rows

# tbl_regression.lmerMod()

    Code
      as.data.frame(tbl_regression(lme4::lmer(mpg ~ hp + (1 | cyl), mtcars)))
    Output
        **Characteristic** **Beta**  **95% CI**
      1                 hp    -0.03 -0.06, 0.00

# tbl_regression.gam()

    Code
      as.data.frame(tbl_regression(gam(mpg ~ s(hp) + factor(cyl), data = mtcars)))
    Output
        **Characteristic** **Beta**  **95% CI** **p-value**
      1        factor(cyl)     <NA>        <NA>        <NA>
      2                  4     <NA>        <NA>        <NA>
      3                  6     -4.5 -8.4, -0.68       0.030
      4                  8     -7.8   -14, -1.4       0.026
      5              s(hp)     <NA>        <NA>       0.093

# tbl_regression.crr()

    Code
      set.seed(10)
      ftime <- rexp(200)
      fstatus <- sample(0:2, 200, replace = TRUE)
      cov <- matrix(runif(600), nrow = 200)
      dimnames(cov)[[2]] <- c("x1", "x2", "x3")
      as.data.frame(tbl_regression(crr(ftime, fstatus, cov)))
    Message
      For better summary support, build model with `tidycmprsk::crr()`.
      Visit <https://mskcc-epi-bio.github.io/tidycmprsk/> for details.
      x Unable to identify the list of variables.
      
      This is usually due to an error calling `stats::model.frame(x)`or `stats::model.matrix(x)`.
      It could be the case if that type of model does not implement these methods.
      Rarely, this error may occur if the model object was created within
      a functional programming framework (e.g. using `lappy()`, `purrr::map()`, etc.).
    Output
        **Characteristic** **log(HR)**  **95% CI** **p-value**
      1                 x1        0.27  -0.56, 1.1         0.5
      2                 x2       -0.06 -0.80, 0.69         0.9
      3                 x3        0.28  -0.47, 1.0         0.5

# tbl_regression.multinom()

    Code
      as.data.frame(tbl_regression(nnet::multinom(cyl ~ am, mtcars)))
    Output
      # weights:  9 (4 variable)
      initial  value 35.155593 
      final  value 29.311125 
      converged
    Message
      i Multinomial models have a different underlying structure than the models gtsummary was designed for.
      * Functions designed to work with `tbl_regression()` objects may yield unexpected results.
    Output
        **Outcome** **Characteristic** **log(OR)**  **95% CI** **p-value**
      1           6                 am        -1.3  -3.3, 0.73         0.2
      2           8                 am        -2.8 -4.8, -0.77       0.007

