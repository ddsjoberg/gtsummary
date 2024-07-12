# add_vif(x)

    Code
      as.data.frame(add_vif(tbl_regression(lm(age ~ marker + grade, trial))))
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

# add_vif(statistic)

    Code
      as.data.frame(add_vif(tbl_regression(lm(age ~ marker + grade, trial)),
      statistic = c("df", "aGVIF")))
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **df** **Adjusted GVIF**
      1 Marker Level (ng/mL)    -0.04  -2.6, 2.5        >0.9      1               1.0
      2                Grade     <NA>       <NA>        <NA>      2               1.0
      3                    I     <NA>       <NA>        <NA>   <NA>              <NA>
      4                   II     0.64  -4.7, 6.0         0.8   <NA>              <NA>
      5                  III      2.4  -2.8, 7.6         0.4   <NA>              <NA>

# add_vif(estimate_fun)

    Code
      as.data.frame(add_vif(tbl_regression(lm(age ~ marker + response, trial)),
      statistic = "VIF", estimate_fun = label_style_sigfig(digits = 5)))
    Output
          **Characteristic** **Beta** **95% CI** **p-value** **VIF**
      1 Marker Level (ng/mL)     0.03  -2.5, 2.6        >0.9  1.0132
      2       Tumor Response      3.9 -0.90, 8.6        0.11  1.0132

