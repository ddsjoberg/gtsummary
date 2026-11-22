# tbl_svycontinuous(data)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1              Grade                 <NA>                  <NA>
      2                  I    0.93 (0.23, 1.71)     1.04 (0.26, 1.48)
      3                 II    0.61 (0.28, 1.23)     0.21 (0.09, 1.08)
      4                III    0.73 (0.16, 1.94)     0.58 (0.33, 1.63)
      5     Tumor Response                 <NA>                  <NA>
      6                  0    0.73 (0.22, 1.41)     0.39 (0.17, 1.15)
      7                  1    0.98 (0.31, 1.89)     0.74 (0.29, 1.33)

---

    Code
      tbl_svycontinuous(trial, variable = age, include = grade)
    Condition
      Error in `tbl_svycontinuous()`:
      ! The `data` argument must be class <survey.design>, not a tibble.

# tbl_svycontinuous(by)

    Code
      tbl_svycontinuous(svy_trial, variable = age, include = grade, by = c(trt,
        response))
    Condition
      Error in `tbl_svycontinuous()`:
      ! The `by` argument must be length 1 or empty.
      i Use `tbl_strata()` for more than one `by` variable.

---

    Code
      as.data.frame(tbl_svycontinuous(svy_trial, variable = age, include = grade, by = response))
    Message
      7 rows with missingness in the "response" column have been removed with `subset()`.
    Output
        **Characteristic** **0**  \nN = 132 **1**  \nN = 61
      1              Grade             <NA>            <NA>
      2                  I      46 (35, 53)     48 (41, 63)
      3                 II      47 (34, 55)     49 (44, 58)
      4                III      45 (37, 57)     49 (43, 58)

# tbl_svycontinuous(statistic)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic**   **N = 200**
      1              Grade          <NA>
      2                  I 46 (2); 66/68
      3                 II 48 (2); 62/68
      4                III 48 (2); 61/64

---

    Code
      tbl_svycontinuous(svy_trial, variable = age, include = grade, statistic = ~
        "{not_a_stat}")
    Condition
      Error in `tbl_svycontinuous()`:
      ! Error in the values of the `statistic` argument for variable "age".
      i Values must be in "mean", "median", "min", "max", "sum", "var", "sd", "mean.std.error", "deff", and "p##"

# tbl_svycontinuous(value, label)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **N = 200**
      1              Grade        <NA>
      2                  I 47 (37, 56)
      3                 II 48 (37, 57)
      4                III 47 (38, 58)
      5     Tumor Response 49 (43, 59)

# tbl_svycontinuous() with a complex design

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **No**  \nN = 1,692 **Yes**  \nN = 4,502
      1              stype                <NA>                 <NA>
      2                  E           632 (102)            654 (107)
      3                  H            637 (75)            600 (118)
      4                  M            632 (98)            631 (121)
      5             awards                <NA>                 <NA>
      6                 No            633 (96)             501 (25)
      7                Yes             NA (NA)            652 (108)

