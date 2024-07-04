# add_p.tbl_svysummary() snapshots of common outputs

    Code
      select(as.data.frame(add_p(tbl_svysummary(svy_trial, by = grade)), col_labels = FALSE),
      -all_stat_cols())
    Output
                          label p.value
      1  Chemotherapy Treatment     0.9
      2                  Drug A    <NA>
      3                  Drug B    <NA>
      4                     Age     0.8
      5                 Unknown    <NA>
      6    Marker Level (ng/mL)   0.016
      7                 Unknown    <NA>
      8                 T Stage     0.6
      9                      T1    <NA>
      10                     T2    <NA>
      11                     T3    <NA>
      12                     T4    <NA>
      13         Tumor Response    >0.9
      14                Unknown    <NA>
      15           Patient Died   0.082
      16 Months to Death/Censor   0.040

---

    Code
      select(as.data.frame(add_p(tbl_svysummary(svy_titanic, by = Survived)),
      col_labels = FALSE), -all_stat_cols())
    Message
      The following warnings were returned during `add_p()`:
      ! For variable `Age` (`Survived`) and "statistic" and "p.value" statistics: Chi-squared approximation may be incorrect
      ! For variable `Class` (`Survived`) and "statistic" and "p.value" statistics: Chi-squared approximation may be incorrect
      ! For variable `Sex` (`Survived`) and "statistic" and "p.value" statistics: Chi-squared approximation may be incorrect
    Output
          label p.value
      1   Class     0.7
      2     1st    <NA>
      3     2nd    <NA>
      4     3rd    <NA>
      5    Crew    <NA>
      6     Sex   0.048
      7    Male    <NA>
      8  Female    <NA>
      9     Age     0.4
      10  Child    <NA>
      11  Adult    <NA>
      12   Freq   0.013

# add_p.tbl_svysummary(x) messaging

    Code
      add_p(tbl_svysummary(svy_trial))
    Condition
      Error in `add_p()`:
      ! Cannot run `add_p()` when `tbl_svysummary(by)` argument not included.

# add_p.tbl_svysummary(test) messaging

    Code
      add_p(tbl_svysummary(svy_trial, by = trt, include = age), test = age ~ letters)
    Condition
      Error in `add_p()`:
      ! Invalid value passed in `test` argument for variable "age".
      i Expecting a <string> or <function>, not a character vector.

