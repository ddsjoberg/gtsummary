# input checks

    Code
      lm(age ~ marker + grade, trial) %>% tbl_regression() %>% modify_column_hide(
        column = ci) %>% modify_column_unhide(column = std.error) %>% as.data.frame()
    Output
          **Characteristic** **Beta** **SE** **p-value**
      1 Marker Level (ng/mL)    -0.04   1.28        >0.9
      2                Grade     <NA>   <NA>        <NA>
      3                    I     <NA>   <NA>        <NA>
      4                   II     0.64   2.70         0.8
      5                  III      2.4   2.64         0.4

