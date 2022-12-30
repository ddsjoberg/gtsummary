# no errors/warnings with tidy_robust()

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

