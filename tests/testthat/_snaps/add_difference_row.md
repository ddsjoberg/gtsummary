# add_difference_row() works

    Code
      as.data.frame(as.data.frame(tbl))
    Output
        **Characteristic** **I**  \nN = 68 **II**  \nN = 68 **III**  \nN = 64
      1                Age     47 (37, 56)      49 (37, 57)       47 (38, 58)
      2    Mean Difference            <NA>             -1.4              -2.0
      3             95% CI            <NA>        -6.4, 3.7         -7.1, 3.2
      4            p-value            <NA>              0.6               0.5
      5     Tumor Response        21 (31%)         19 (30%)          21 (33%)
      6    Rate Difference            <NA>             1.2%             -2.0%
      7             95% CI            <NA>        -16%, 18%         -20%, 16%
      8            p-value            <NA>             >0.9              >0.9

# add_difference_row() messaging

    Code
      add_difference_row(tbl_summary(trial, by = grade, include = c(age, response),
      missing = "no"), reference = "XXX")
    Condition
      Error in `add_difference_row()`:
      ! The `reference` argument must be one of "I", "II", and "III".

