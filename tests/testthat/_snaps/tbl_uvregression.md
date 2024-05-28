# tbl_uvregression(x)

    Code
      as.data.frame(tbl1)
    Output
                 **Outcome** **N** **Beta**  **95% CI** **p-value**
      1 Marker Level (ng/mL)   190    -0.20 -0.44, 0.05        0.12
      2                  Age   189     0.44   -3.7, 4.6         0.8

# tbl_uvregression(method)

    Code
      as.data.frame(tbl1)
    Output
          **Characteristic** **N** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)   179    -0.05  -2.5, 2.4        >0.9
      2                Grade   189     <NA>       <NA>        <NA>
      3                    I  <NA>     <NA>       <NA>        <NA>
      4                   II  <NA>      1.4  -3.6, 6.4         0.6
      5                  III  <NA>      2.0  -3.1, 7.0         0.4

# tbl_uvregression(method.args)

    Code
      as.data.frame(tbl2)
    Output
            **Characteristic** **N** **log(HR)**  **95% CI** **p-value**
      1                    Age   189        0.01  0.01, 0.01      <0.001
      2 Chemotherapy Treatment   200        <NA>        <NA>        <NA>
      3                 Drug A  <NA>        <NA>        <NA>        <NA>
      4                 Drug B  <NA>        0.29 -0.12, 0.70         0.2

# tbl_uvregression(include)

    Code
      as.data.frame(tbl1)
    Output
                 **Outcome** **N** **Beta**  **95% CI** **p-value**
      1                  Age   189     0.44   -3.7, 4.6         0.8
      2 Marker Level (ng/mL)   190    -0.20 -0.44, 0.05        0.12

---

    Code
      as.data.frame(tbl2)
    Output
          **Characteristic** **N** **HR** **95% CI** **p-value**
      1                  Age   189   1.01 0.99, 1.02         0.3
      2 Marker Level (ng/mL)   190   0.91 0.72, 1.15         0.4

# tbl_uvregression(tidy_fun)

    Code
      tbl1 <- tbl_uvregression(trial, y = age, method = lm, include = marker,
        tidy_fun = function(x, ...) {
          cat("THIS IS MY CUSTOM MESSAGE!")
          broom::tidy(x, ...)
        })
    Output
      THIS IS MY CUSTOM MESSAGE!

# tbl_uvregression(estimate_fun)

    Code
      as.data.frame(tbl_uvregression(trial, y = age, method = lm, include = c(marker,
        trt), estimate_fun = styfn_sigfig(digits = 4)), col_label = FALSE)
    Output
                         label stat_n estimate      conf.low p.value
      1   Marker Level (ng/mL)    179  -0.0545 -2.533, 2.424    >0.9
      2 Chemotherapy Treatment    189     <NA>          <NA>    <NA>
      3                 Drug A   <NA>     <NA>          <NA>    <NA>
      4                 Drug B   <NA>   0.4380 -3.683, 4.559     0.8

# tbl_uvregression(pvalue_fun)

    Code
      as.data.frame(tbl_uvregression(trial, y = age, method = lm, include = c(marker,
        trt), pvalue_fun = styfn_pvalue(digits = 3)), col_label = FALSE)
    Output
                         label stat_n estimate  conf.low p.value
      1   Marker Level (ng/mL)    179    -0.05 -2.5, 2.4   0.965
      2 Chemotherapy Treatment    189     <NA>      <NA>    <NA>
      3                 Drug A   <NA>     <NA>      <NA>    <NA>
      4                 Drug B   <NA>     0.44 -3.7, 4.6   0.834

# tbl_uvregression(conf.int)

    Code
      as.data.frame(tbl_uvregression(trial, y = response, method = glm, method.args = list(
        family = binomial), include = trt, conf.int = FALSE))
    Output
            **Characteristic** **N** **log(OR)** **p-value**
      1 Chemotherapy Treatment   193        <NA>        <NA>
      2                 Drug A  <NA>        <NA>        <NA>
      3                 Drug B  <NA>        0.19         0.5

