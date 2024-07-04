# add_difference.tbl_svysummary() snapshots of common outputs

    Code
      select(as.data.frame(add_difference(tbl_svysummary(svy_trial, by = trt)),
      col_labels = FALSE), -all_stat_cols())
    Output
                          label estimate    conf.low p.value
      1                     Age     0.44   -3.7, 4.6     0.8
      2                 Unknown     <NA>        <NA>    <NA>
      3    Marker Level (ng/mL)    -0.20 -0.44, 0.05    0.12
      4                 Unknown     <NA>        <NA>    <NA>
      5                 T Stage     0.12 -0.16, 0.40    <NA>
      6                      T1     <NA>        <NA>    <NA>
      7                      T2     <NA>        <NA>    <NA>
      8                      T3     <NA>        <NA>    <NA>
      9                      T4     <NA>        <NA>    <NA>
      10                  Grade     0.07 -0.20, 0.35    <NA>
      11                      I     <NA>        <NA>    <NA>
      12                     II     <NA>        <NA>    <NA>
      13                    III     <NA>        <NA>    <NA>
      14         Tumor Response    -4.2%  -17%, 8.9%     0.5
      15                Unknown     <NA>        <NA>    <NA>
      16           Patient Died    -5.8%  -20%, 8.0%     0.4
      17 Months to Death/Censor     -1.2  -2.7, 0.26    0.11

---

    Code
      select(as.data.frame(add_difference(tbl_svysummary(svy_titanic, by = Survived)),
      col_labels = FALSE), -all_stat_cols())
    Message
      The following warnings were returned during `add_difference()`:
      ! For variable `Freq` (`Survived`) and "estimate", "statistic", "p.value", "parameter", "conf.low", and "conf.high" statistics: observations with zero weight not used for calculating dispersion
      ! For variable `Freq` (`Survived`) and "estimate", "statistic", "p.value", "parameter", "conf.low", and "conf.high" statistics: observations with zero weight not used for calculating dispersion
    Output
          label estimate   conf.low p.value
      1   Class     0.56 0.47, 0.65    <NA>
      2     1st     <NA>       <NA>    <NA>
      3     2nd     <NA>       <NA>    <NA>
      4     3rd     <NA>       <NA>    <NA>
      5    Crew     <NA>       <NA>    <NA>
      6     Sex     0.80 0.71, 0.89    <NA>
      7    Male     <NA>       <NA>    <NA>
      8  Female     <NA>       <NA>    <NA>
      9     Age     0.09 0.00, 0.18    <NA>
      10  Child     <NA>       <NA>    <NA>
      11  Adult     <NA>       <NA>    <NA>
      12   Freq     -323  -573, -73   0.014

# add_difference.tbl_svysummary(x) messaging

    Code
      add_difference(tbl_svysummary(svy_trial, include = age))
    Condition
      Error in `add_difference()`:
      ! Cannot run `add_difference()` when `tbl_summary(by)` column does not have exactly two levels.

---

    Code
      tbl <- add_difference(tbl_svysummary(svy_trial, by = trt, percent = "row",
        include = grade))
    Condition
      Warning:
      The `add_difference()` results for categorical variables may not compatible with `tbl_summary(percent = c('cell', 'row'))`.
      i Use column percentages instead, `tbl_summary(percent = 'column')`.

# add_difference.tbl_svysummary() + add_p.tbl_svysummary()

    Code
      dplyr::select(as.data.frame(add_p(add_difference(tbl_svysummary(svy_trial, by = trt,
        include = c(age, response, grade), missing = "no"), test = ~"smd")),
      col_label = FALSE), -all_stat_cols())
    Output
                 label estimate    conf.low p.value
      1            Age    -0.03 -0.32, 0.25     0.7
      2 Tumor Response    -0.09 -0.37, 0.19     0.5
      3          Grade     0.07 -0.20, 0.35     0.9
      4              I     <NA>        <NA>    <NA>
      5             II     <NA>        <NA>    <NA>
      6            III     <NA>        <NA>    <NA>

# add_difference.tbl_svysummary(test) messaging

    Code
      add_difference(tbl_svysummary(svy_trial, by = trt, include = age), test = age ~
        letters)
    Condition
      Error in `add_difference()`:
      ! Invalid value passed in `test` argument for variable "age".
      i Expecting a <string> or <function>, not a character vector.

