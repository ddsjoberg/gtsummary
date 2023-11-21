# add_p.tbl_continuous() works

    Code
      tbl_continuous(data = trial, variable = age, by = trt, include = grade) %>%
        add_p() %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1              Grade               <NA>                <NA>         0.9
      2                  I        46 (36, 60)         48 (42, 55)        <NA>
      3                 II        45 (31, 55)         51 (43, 57)        <NA>
      4                III        52 (42, 60)         45 (36, 52)        <NA>

---

    Code
      tbl_continuous(data = trial, variable = age, include = grade) %>% add_p() %>%
        as.data.frame()
    Output
        **Characteristic** **N = 200** **p-value**
      1              Grade        <NA>         0.8
      2                  I 47 (37, 56)        <NA>
      3                 II 49 (37, 57)        <NA>
      4                III 47 (38, 58)        <NA>

---

    Code
      tbl_continuous(data = trial, variable = age, include = trt) %>% add_p(
        everything() ~ "t.test") %>% as.data.frame()
    Output
            **Characteristic** **N = 200** **p-value**
      1 Chemotherapy Treatment        <NA>         0.8
      2                 Drug A 46 (37, 59)        <NA>
      3                 Drug B 48 (39, 56)        <NA>

---

    Code
      tbl_continuous(data = trial, variable = age, include = trt) %>% add_p(
        everything() ~ "wilcox.test") %>% as.data.frame()
    Output
            **Characteristic** **N = 200** **p-value**
      1 Chemotherapy Treatment        <NA>         0.7
      2                 Drug A 46 (37, 59)        <NA>
      3                 Drug B 48 (39, 56)        <NA>

---

    Code
      tbl_continuous(data = trial, variable = age, include = trt) %>% add_p(
        everything() ~ "lme4", group = "stage") %>% as.data.frame()
    Message
      boundary (singular) fit: see help('isSingular')
      Warning for variable 'trt':
      simpleWarning in (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf, : failure to converge in 10000 evaluations
      Warning for variable 'trt':
      simpleWarning in optwrap(optimizer, devfun, start, rho$lower, control = control, : convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations
      boundary (singular) fit: see help('isSingular')
    Output
            **Characteristic** **N = 200** **p-value**
      1 Chemotherapy Treatment        <NA>         0.5
      2                 Drug A 46 (37, 59)        <NA>
      3                 Drug B 48 (39, 56)        <NA>

