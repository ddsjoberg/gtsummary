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
      1                    Age   183        0.01  0.01, 0.01      <0.001
      2 Chemotherapy Treatment   193        <NA>        <NA>        <NA>
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

---

    Code
      as.data.frame(tbl_uvregression(dplyr::select(mtcars, `M P G` = mpg, hp), y = hp,
      method = lm))
    Output
        **Characteristic** **N** **Beta** **95% CI** **p-value**
      1              M P G    32     -8.8  -12, -6.2      <0.001

---

    Code
      as.data.frame(tbl_uvregression(dplyr::select(mtcars, `M P G` = mpg, hp), x = hp,
      method = lm))
    Output
        **Outcome** **N** **Beta**   **95% CI** **p-value**
      1       M P G    32    -0.07 -0.09, -0.05      <0.001

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
        trt), estimate_fun = label_style_sigfig(digits = 4)), col_label = FALSE)
    Output
                         label stat_n estimate      conf.low p.value
      1   Marker Level (ng/mL)    179  -0.0545 -2.533, 2.424    >0.9
      2 Chemotherapy Treatment    189     <NA>          <NA>    <NA>
      3                 Drug A   <NA>     <NA>          <NA>    <NA>
      4                 Drug B   <NA>   0.4380 -3.683, 4.559     0.8

# tbl_uvregression(pvalue_fun)

    Code
      as.data.frame(tbl_uvregression(trial, y = age, method = lm, include = c(marker,
        trt), pvalue_fun = label_style_pvalue(digits = 3)), col_label = FALSE)
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

# tbl_uvregression(...)

    Code
      as.data.frame(tbl_uvregression(trial, y = response, method = glm, method.args = list(
        family = binomial), include = trt, add_header_rows = FALSE))
    Output
        **Characteristic** **N** **log(OR)**  **95% CI** **p-value**
      1             Drug A   193        <NA>        <NA>        <NA>
      2             Drug B   193        0.19 -0.41, 0.81         0.5

# tbl_uvregression(x,y) messaging

    Code
      tbl_uvregression(trial, method = lm, include = trt)
    Condition
      Error in `tbl_uvregression()`:
      ! Must specify one and only one of arguments `x` and `y`.

---

    Code
      tbl_uvregression(trial, x = age, y = marker, method = lm, include = trt)
    Condition
      Error in `tbl_uvregression()`:
      ! Must specify one and only one of arguments `x` and `y`.

# tbl_uvregression(formula) messaging

    Code
      tbl_uvregression(trial, y = age, method = lm, include = trt, formula = "{y} ~ {y}")
    Condition
      Error in `tbl_uvregression()`:
      ! Error in argument `formula` structure.
      i The substring "{y}" must appear once in the string and it must be on the LHS of the formula.

---

    Code
      tbl_uvregression(trial, y = age, method = lm, include = trt, formula = "{y} ~ {x} + {x}")
    Condition
      Error in `tbl_uvregression()`:
      ! Error in argument `formula` structure.
      i The substring "{x}" must appear once in the string and it must be on the RHS of the formula.

---

    Code
      tbl_uvregression(trial, y = age, method = lm, include = trt, formula = "{y} {x}")
    Condition
      Error in `tbl_uvregression()`:
      ! The `formula` argument must be have structure of a standard formula, e.g. "{y} ~ {x}".

---

    Code
      tbl_uvregression(dplyr::rename(trial, `Tx Effect` = trt), y = "Tx Effect",
      method = glm, method.args = list(family = binomial), include = age)
    Condition
      Error in `tbl_uvregression()`:
      ! There was an error constructing the formula for variable "age". See message below.
      x <text>:1:4: unexpected symbol 1: Tx Effect ^

# tbl_uvregression(method.args) messaging

    Code
      tbl_uvregression(trial, y = response, method = glm, method.args = list(
        not_an_arg = FALSE), include = trt)
    Condition
      Error in `tbl_uvregression()`:
      ! There was an error constructing the model for variable "trt". See message below.
      x There was an error evaluating the model Caused by error in `glm.control()`: ! unused argument (not_an_arg = FALSE)

---

    Code
      tbl <- tbl_uvregression(trial, y = age, method = lm, method.args = list(
        not_an_arg = FALSE), include = trt)
    Message
      There was a warning constructing the model for variable "trt". See message below.
      ! In lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : extra argument 'not_an_arg' will be disregarded

# tbl_uvregression() messaging

    Code
      tbl_uvregression(trial, y = age, method = lm, include = trt, tidy_fun = function(
        x, ...) stop("Inducing an error"))
    Condition
      Error in `tbl_uvregression()`:
      ! There was an error running `tbl_regression()` for variable "trt". See message below.
      x Error in (function (x, ...) : Inducing an error

---

    Code
      tbl <- tbl_uvregression(trial, y = age, method = lm, include = trt, tidy_fun = function(
        x, ...) {
        warning("Inducing an warning")
        broom::tidy(x, ...)
      })
    Message
      There was a warning running `tbl_regression()` for variable "trt". See message below.
      ! Inducing an warning

