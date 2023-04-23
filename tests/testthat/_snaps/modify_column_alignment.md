# modify_column_alignment() works

    Code
      lm(age ~ marker + grade, trial) %>% tbl_regression() %>%
        modify_column_alignment(columns = everything(), align = "left") %>%
        as.data.frame()
    Output
          **Characteristic** **Beta** **95% CI** **p-value**
      1 Marker Level (ng/mL)    -0.04  -2.6, 2.5        >0.9
      2                Grade     <NA>       <NA>        <NA>
      3                    I     <NA>       <NA>        <NA>
      4                   II     0.64  -4.7, 6.0         0.8
      5                  III      2.4  -2.8, 7.6         0.4

