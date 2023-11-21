# checks for rows arg

    Code
      tbl_summary(trial[c("trt", "age")]) %>% modify_table_styling(columns = label,
        footnote = "test footnote", rows = variable == "age") %>% as.data.frame()
    Output
            **Characteristic** **N = 200**
      1 Chemotherapy Treatment        <NA>
      2                 Drug A    98 (49%)
      3                 Drug B   102 (51%)
      4                    Age 47 (38, 57)
      5                Unknown          11

---

    Code
      tbl_summary(trial[c("trt", "age")]) %>% modify_table_styling(columns = label,
        footnote = "test footnote", rows = variable == footnote_variable) %>%
        as.data.frame()
    Output
            **Characteristic** **N = 200**
      1 Chemotherapy Treatment        <NA>
      2                 Drug A    98 (49%)
      3                 Drug B   102 (51%)
      4                    Age 47 (38, 57)
      5                Unknown          11

---

    Code
      tbl_summary(trial[c("trt", "age")]) %>% modify_table_styling(columns = label,
        footnote = "test footnote", rows = null_value) %>% as.data.frame()
    Output
            **Characteristic** **N = 200**
      1 Chemotherapy Treatment        <NA>
      2                 Drug A    98 (49%)
      3                 Drug B   102 (51%)
      4                    Age 47 (38, 57)
      5                Unknown          11

---

    Code
      tbl1 %>% as.data.frame()
    Output
        **Characteristic**            **Drug A**, N = 98 **Difference** **95% CI**
      1                Age 46 (37, 59)  ---  48 (39, 56)          -0.44  -4.6, 3.7
      2     Tumor Response       28 (29%)  ---  33 (34%)          -4.2% -18%, 9.9%
        **p-value**
      1         0.8
      2         0.6

---

    Code
      tbl2 %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **Difference**
      1                Age        46 (37, 59)         48 (39, 56)          -0.44
      2     Tumor Response           28 (29%)            33 (34%)          -4.2%
        **95% CI** **p-value**
      1  -4.6, 3.7         0.8
      2 -18%, 9.9%         0.6

