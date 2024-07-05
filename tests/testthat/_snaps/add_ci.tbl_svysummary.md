# add_ci(method) after `add_overall()`

    Code
      dplyr::select(as.data.frame(add_ci(add_overall(tbl_svysummary(svy_trial, by = trt,
        include = c(age, grade), missing = "no", statistic = list(all_continuous() ~
          "{mean}", all_categorical() ~ "{p}%")))), col_label = FALSE),
      -all_stat_cols())
    Output
        label ci_stat_0 ci_stat_1 ci_stat_2
      1   Age    45, 49    44, 50    45, 50
      2 Grade      <NA>      <NA>      <NA>
      3     I  28%, 41%  27%, 46%  24%, 42%
      4    II  28%, 41%  24%, 43%  27%, 45%
      5   III  26%, 39%  23%, 42%  24%, 42%

# add_ci(include)

    Code
      dplyr::select(as.data.frame(add_ci(add_overall(tbl_svysummary(svy_trial,
        include = c(age, grade), by = trt, missing = "no")), include = age),
      col_label = FALSE), -all_stat_cols())
    Message
      A confidence interval for the mean in variable "age" was requested; however, the primary table does not contain a mean.
    Output
        label ci_stat_0 ci_stat_1 ci_stat_2
      1   Age    45, 49    44, 50    45, 50
      2 Grade      <NA>      <NA>      <NA>
      3     I      <NA>      <NA>      <NA>
      4    II      <NA>      <NA>      <NA>
      5   III      <NA>      <NA>      <NA>

# add_ci(statistic)

    Code
      as.data.frame(add_ci(tbl_svysummary(svy_trial, include = c(age, grade),
      missing = "no"), statistic = list(all_continuous() ~ "{conf.low} - {conf.high}",
      all_categorical() ~ "{conf.low}% - {conf.high}%")))
    Message
      A confidence interval for the mean in variable "age" was requested; however, the primary table does not contain a mean.
    Output
        **Characteristic** **N = 200** **95% CI**
      1                Age 47 (38, 57)    45 - 49
      2              Grade        <NA>       <NA>
      3                  I    68 (34%)  28% - 41%
      4                 II    68 (34%)  28% - 41%
      5                III    64 (32%)  26% - 39%

# add_ci(pattern)

    Code
      as.data.frame(add_ci(tbl_svysummary(svy_trial, include = c(age, grade),
      missing = "no", statistic = list(age = "{mean}")), include = age, pattern = "{stat} [{ci}]"))
    Output
        **Characteristic** **N = 200** [**95% CI**]
      1                Age              47 [45, 49]
      2              Grade                     <NA>
      3                  I                 68 (34%)
      4                 II                 68 (34%)
      5                III                 64 (32%)

# add_ci(pattern) messaging

    Code
      add_ci(tbl_svysummary(svy_trial, include = age, missing = "no", statistic = list(
        age = "{mean}")), pattern = "{not_a_stat} [{ci}]")
    Condition
      Error in `add_ci()`:
      ! The `pattern` argument allows only for elements "stat" and "ci" to be included in curly brackets.

---

    Code
      add_ci(tbl_svysummary(svy_trial, include = age, missing = "no", statistic = list(
        age = "{mean}")), pattern = "{ci}")
    Condition
      Error in `add_ci()`:
      ! The `pattern` argument must include references to both "{stat}" and "{ci}"

# add_ci(method) messaging

    Code
      add_ci(tbl_svysummary(svy_trial, include = age, missing = "no", statistic = list(
        age = "{mean}")), method = list(age = "xxxxxxxxxx"))
    Condition
      Error in `add_ci()`:
      ! The value of the `method` argument for continuous variable "age" must be one of "svymean", "svymedian", "svymedian.mean", "svymedian.beta", "svymedian.xlogit", "svymedian.asin", and "svymedian.score"

---

    Code
      add_ci(tbl_svysummary(svy_trial, include = grade), method = list(grade = "svymean"))
    Condition
      Error in `add_ci()`:
      ! The value of the `method` argument for categorical variable "grade" must be one of "svyprop", "svyprop.logit", "svyprop.likelihood", "svyprop.asin", "svyprop.beta", "svyprop.mean", and "svyprop.xlogit"

# add_ci() correctly handles dichotomous variables

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **N = 200** **95% CI**
      1     Tumor Response   132 (68%)   61%, 75%
      2              Grade    64 (32%)   26%, 39%

