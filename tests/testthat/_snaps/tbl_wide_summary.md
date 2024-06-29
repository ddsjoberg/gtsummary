# tbl_wide_summary(data)

    Code
      as.data.frame(tbl_wide_summary(trial, include = c(response, grade)))
    Output
        **Characteristic** **n** **%**
      1     Tumor Response    61   32%
      2              Grade  <NA>  <NA>
      3                  I    68   34%
      4                 II    68   34%
      5                III    64   32%

# tbl_wide_summary(statistic)

    Code
      as.data.frame(tbl_wide_summary(trial, include = c(age, marker), statistic = c(
        "{mean}", "{sd}")))
    Output
          **Characteristic** **Mean** **SD**
      1                  Age       47     14
      2 Marker Level (ng/mL)     0.92   0.86

