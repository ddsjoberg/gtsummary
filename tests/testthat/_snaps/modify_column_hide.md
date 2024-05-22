# modify_column_hide() works

    Code
      as.data.frame(modify_column_unhide(modify_column_hide(tbl_regression(lm(age ~
        marker + grade, trial)), column = conf.low), column = std.error))
    Output
          **Characteristic** **Beta** **SE** **p-value**
      1 Marker Level (ng/mL)    -0.04   1.28        >0.9
      2                Grade     <NA>   <NA>        <NA>
      3                    I     <NA>   <NA>        <NA>
      4                   II     0.64   2.70         0.8
      5                  III      2.4   2.64         0.4

