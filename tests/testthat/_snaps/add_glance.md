# add_glance_table(x)

    Code
      as.data.frame(add_glance_table(tbl_regression(lm(age ~ trt, trial))))
    Output
             **Characteristic** **Beta** **95% CI** **p-value**
      1  Chemotherapy Treatment     <NA>       <NA>        <NA>
      2                  Drug A     <NA>       <NA>        <NA>
      3                  Drug B     0.44  -3.7, 4.6         0.8
      4                      R²    0.000       <NA>        <NA>
      5             Adjusted R²   -0.005       <NA>        <NA>
      6                   Sigma     14.3       <NA>        <NA>
      7               Statistic    0.044       <NA>        <NA>
      8                 p-value      0.8       <NA>        <NA>
      9                      df        1       <NA>        <NA>
      10         Log-likelihood     -771       <NA>        <NA>
      11                    AIC    1,547       <NA>        <NA>
      12                    BIC    1,557       <NA>        <NA>
      13               Deviance   38,499       <NA>        <NA>
      14            Residual df      187       <NA>        <NA>
      15               No. Obs.      189       <NA>        <NA>

