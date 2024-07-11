# tidy_standardize()

    Code
      tidy_standardize(lm(age ~ grade + marker, trial))
    Output
      # A tibble: 4 x 4
        term        estimate conf.low conf.high
        <chr>          <dbl>    <dbl>     <dbl>
      1 (Intercept) -0.0676    -0.317     0.182
      2 gradeII      0.0440    -0.324     0.412
      3 gradeIII     0.165     -0.195     0.526
      4 marker      -0.00227   -0.154     0.149

# tidy_bootstrap()

    Code
      tidy_bootstrap(lm(age ~ grade + marker, trial), exponentiate = TRUE)
    Output
      # A tibble: 4 x 5
        term        estimate conf.low conf.high p.value
        <chr>          <dbl>    <dbl>     <dbl>   <dbl>
      1 (Intercept) 1.05e+20 7.25e+17   1.31e22   0    
      2 gradeII     1.79e+ 0 9.76e- 3   2.94e 2   0.856
      3 gradeIII    1.27e+ 1 8.37e- 2   2.39e 3   0.35 
      4 marker      8.93e- 1 8.51e- 2   1.01e 1   0.918

# tidy_wald_test()

    Code
      tidy_wald_test(lm(age ~ grade + marker, trial), exponentiate = TRUE)
    Output
      # A tibble: 3 x 4
        term           df  statistic p.value
        <chr>       <dbl>      <dbl>   <dbl>
      1 (Intercept)     1 405.         0    
      2 grade           2   0.869      0.648
      3 marker          1   0.000875   0.976

# tidy_robust()

    Code
      tidy_robust(lm(age ~ grade + marker, trial), exponentiate = TRUE)
    Message
      i Arguments `vcov` and `vcov_args` have not been specified in `tidy_robust()`. Specify at least one to obtain robust standard errors.
    Output
               term     estimate std.error conf.level     conf.low    conf.high
      1 (Intercept) 1.005084e+20  2.289965       0.95 1.094981e+18 9.225677e+21
      2     gradeII 1.889855e+00  2.695779       0.95 9.242676e-03 3.864196e+02
      3    gradeIII 1.095389e+01  2.642808       0.95 5.947585e-02 2.017419e+03
      4      marker 9.627819e-01  1.281978       0.95 7.668449e-02 1.208783e+01
          statistic df.error      p.value
      1 20.11243136      175 2.263683e-47
      2  0.23610980      175 8.136236e-01
      3  0.90573903      175 3.663193e-01
      4 -0.02958585      175 9.764311e-01

