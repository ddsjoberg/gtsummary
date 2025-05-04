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

# add_difference_row(reference) messaging

    Code
      add_difference_row(tbl_summary(trial, by = grade, include = c(age, response),
      missing = "no"), reference = "XXX")
    Condition
      Error in `add_difference_row()`:
      ! The `reference` argument must be one of "I", "II", and "III".

# add_difference_row() messaging

    Code
      add_difference_row(tbl_summary(trial, include = age), reference = "I")
    Condition
      Error in `add_difference_row()`:
      ! Cannot run `add_difference_row()` when `tbl_summary()` does not include a `by` argument.

---

    Code
      tbl <- add_difference_row(tbl_summary(trial, by = grade, include = response,
        percent = "row"), reference = "I")
    Condition
      Warning:
      The `add_difference_row()` results for categorical variables may not compatible with `tbl_summary(percent = c('cell', 'row'))`.
      i Use column percentages instead, `tbl_summary(percent = 'column')`.

# add_difference_row(test) messaging

    Code
      add_difference_row(tbl_summary(trial, by = trt, include = age), reference = "Drug A",
      test = age ~ (function(...) letters))
    Condition
      Error in `add_difference_row()`:
      ! The result from the `test` argument for variable `age` must be an ARD of class <card> or a data frame with one row.
      i Review `?gtsummary::tests()` for details on constructing a custom function.

---

    Code
      tbl <- add_difference_row(tbl_summary(trial, by = trt, include = age),
      reference = "Drug A", test = age ~ (function(...) stop("oy!")))
    Message
      The following errors were returned during `add_difference_row()`:
      x For variable `age` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: oy!

# add_difference_row.tbl_summary(group)

    Code
      as.data.frame(tbl_groups)
    Output
        **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1                Age          46 (37, 60)           48 (39, 56)
      2        Coefficient                 <NA>                 -0.57

