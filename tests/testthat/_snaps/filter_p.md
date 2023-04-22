# no errors/warnings with standard use after tbl_summary() and add_p()

    Code
      filter_p(table1, t = 0.2) %>% as.data.frame()
    Output
            **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1   Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)       0.085
      2                Unknown                  6                   4        <NA>
      3 Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)        0.14

# no errors/warnings with standard use after tbl_regression() and add_global_p()

    Code
      filter_p(regress1, t = 0.2) %>% as.data.frame()
    Output
        **Characteristic** **OR** **95% CI** **p-value**
      1                Age   1.02 1.00, 1.04       0.092

---

    Code
      filter_p(regress2, t = 0.2) %>% as.data.frame()
    Output
        **Characteristic** **N** **OR** **95% CI** **p-value**
      1                Age   183   1.02 1.00, 1.04       0.091

