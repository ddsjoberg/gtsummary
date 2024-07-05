# add_ci(include)

    Code
      as.data.frame(add_ci(add_overall(tbl_summary(trial, include = c(age, grade),
      by = trt, missing = "no")), include = age))
    Message
      A confidence interval for the mean in variable "age" was requested; however, the primary table does not contain a mean.
    Output
        **Characteristic** **Overall**  \nN = 200 **95% CI** **Drug A**  \nN = 98
      1                Age            47 (38, 57)     45, 49          46 (37, 60)
      2              Grade                   <NA>       <NA>                 <NA>
      3                  I               68 (34%)       <NA>             35 (36%)
      4                 II               68 (34%)       <NA>             32 (33%)
      5                III               64 (32%)       <NA>             31 (32%)
        **95% CI** **Drug B**  \nN = 102 **95% CI**
      1     44, 50           48 (39, 56)     45, 50
      2       <NA>                  <NA>       <NA>
      3       <NA>              33 (32%)       <NA>
      4       <NA>              36 (35%)       <NA>
      5       <NA>              33 (32%)       <NA>

# add_ci(statistic)

    Code
      as.data.frame(add_ci(tbl_summary(trial, include = c(age, grade), by = trt,
      missing = "no"), statistic = list(all_continuous() ~ "{conf.low} - {conf.high}",
      all_categorical() ~ "{conf.low}% - {conf.high}%")))
    Message
      A confidence interval for the mean in variable "age" was requested; however, the primary table does not contain a mean.
    Output
        **Characteristic** **Drug A**  \nN = 98 **95% CI** **Drug B**  \nN = 102
      1                Age          46 (37, 60)    44 - 50           48 (39, 56)
      2              Grade                 <NA>       <NA>                  <NA>
      3                  I             35 (36%)  26% - 46%              33 (32%)
      4                 II             32 (33%)  24% - 43%              36 (35%)
      5                III             31 (32%)  23% - 42%              33 (32%)
        **95% CI**
      1    45 - 50
      2       <NA>
      3  24% - 42%
      4  26% - 45%
      5  24% - 42%

# add_ci(pattern)

    Code
      as.data.frame(add_ci(tbl_summary(trial, include = c(age, grade), missing = "no",
      statistic = list(age = "{mean}")), include = age, pattern = "{stat} [{ci}]"))
    Output
        **Characteristic** **N = 200** [**95% CI**]
      1                Age              47 [45, 49]
      2              Grade                     <NA>
      3                  I                 68 (34%)
      4                 II                 68 (34%)
      5                III                 64 (32%)

# add_ci(pattern) messaging

    Code
      add_ci(tbl_summary(trial, include = age, missing = "no", statistic = list(age = "{mean}")),
      pattern = "{not_a_stat} [{ci}]")
    Condition
      Error in `add_ci()`:
      ! The `pattern` argument allows only for elements "stat" and "ci" to be included in curly brackets.

---

    Code
      add_ci(tbl_summary(trial, include = age, missing = "no", statistic = list(age = "{mean}")),
      pattern = "{ci}")
    Condition
      Error in `add_ci()`:
      ! The `pattern` argument must include references to both "{stat}" and "{ci}"

# add_ci(method) messaging

    Code
      add_ci(tbl_summary(trial, include = age, missing = "no", statistic = list(age = "{mean}")),
      method = list(age = "wilson"))
    Condition
      Error in `add_ci()`:
      ! The value of the `method` argument for continuous variable "age" must be one of "t.test" and "wilcox.test"

---

    Code
      add_ci(tbl_summary(trial, include = grade), method = list(grade = "t.test"))
    Condition
      Error in `add_ci()`:
      ! The value of the `method` argument for categorical variable "grade" must be one of "wald", "wald.no.correct", "exact", "wilson", "wilson.no.correct", "agresti.coull", and "jeffreys"

# add_ci() correctly handles dichotomous variables

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **N = 200** **95% CI**
      1     Tumor Response   132 (68%)   61%, 75%
      2              Grade    64 (32%)   26%, 39%

