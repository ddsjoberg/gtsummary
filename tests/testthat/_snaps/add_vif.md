# no errors/warnings with standard

    Code
      lm(age ~ marker + grade, trial) %>% tbl_regression() %>% add_vif() %>%
        as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **GVIF**
      1 Marker Level (ng/mL)    -0.04  -2.6, 2.5        >0.9      1.0
      2                Grade     <NA>       <NA>        <NA>      1.0
      3                    I     <NA>       <NA>        <NA>     <NA>
      4                   II     0.64  -4.7, 6.0         0.8     <NA>
      5                  III      2.4  -2.8, 7.6         0.4     <NA>
        **Adjusted GVIF**
      1               1.0
      2               1.0
      3              <NA>
      4              <NA>
      5              <NA>

---

    Code
      lm(age ~ marker + response, trial) %>% tbl_regression() %>% add_vif() %>%
        as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **VIF**
      1 Marker Level (ng/mL)     0.03  -2.5, 2.6        >0.9     1.0
      2       Tumor Response      3.9 -0.90, 8.6        0.11     1.0

---

    Code
      lm(age ~ marker + grade, trial) %>% tbl_regression() %>% add_vif(statistic = "aGVIF") %>%
        as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **Adjusted GVIF**
      1 Marker Level (ng/mL)    -0.04  -2.6, 2.5        >0.9               1.0
      2                Grade     <NA>       <NA>        <NA>               1.0
      3                    I     <NA>       <NA>        <NA>              <NA>
      4                   II     0.64  -4.7, 6.0         0.8              <NA>
      5                  III      2.4  -2.8, 7.6         0.4              <NA>

---

    Code
      lm(age ~ marker + grade, trial) %>% tbl_regression() %>% add_vif(statistic = c(
        "aGVIF", "df")) %>% as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **Adjusted GVIF** **df**
      1 Marker Level (ng/mL)    -0.04  -2.6, 2.5        >0.9               1.0      1
      2                Grade     <NA>       <NA>        <NA>               1.0      2
      3                    I     <NA>       <NA>        <NA>              <NA>   <NA>
      4                   II     0.64  -4.7, 6.0         0.8              <NA>   <NA>
      5                  III      2.4  -2.8, 7.6         0.4              <NA>   <NA>

---

    Code
      lm(age ~ marker + response, trial) %>% tbl_regression() %>% add_vif(statistic = "VIF") %>%
        as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **VIF**
      1 Marker Level (ng/mL)     0.03  -2.5, 2.6        >0.9     1.0
      2       Tumor Response      3.9 -0.90, 8.6        0.11     1.0

---

    Code
      lm(age ~ marker + response, trial) %>% tbl_regression() %>% add_vif(statistic = "VIF") %>%
        as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **VIF**
      1 Marker Level (ng/mL)     0.03  -2.5, 2.6        >0.9     1.0
      2       Tumor Response      3.9 -0.90, 8.6        0.11     1.0

