# add_p.tbl_summary() snapshots of common outputs

    Code
      select(as.data.frame(add_p(tbl_summary(trial, by = grade)), col_labels = FALSE),
      -all_stat_cols())
    Output
                          label p.value
      1  Chemotherapy Treatment     0.9
      2                  Drug A    <NA>
      3                  Drug B    <NA>
      4                     Age     0.8
      5                 Unknown    <NA>
      6    Marker Level (ng/mL)   0.019
      7                 Unknown    <NA>
      8                 T Stage     0.6
      9                      T1    <NA>
      10                     T2    <NA>
      11                     T3    <NA>
      12                     T4    <NA>
      13         Tumor Response    >0.9
      14                Unknown    <NA>
      15           Patient Died   0.080
      16 Months to Death/Censor   0.060

---

    Code
      as.data.frame(add_p(tbl_summary(mtcars, by = am, include = c(cyl, gear, vs))))
    Output
        **Characteristic** **0**  \nN = 19 **1**  \nN = 13 **p-value**
      1                cyl            <NA>            <NA>       0.009
      2                  4         3 (16%)         8 (62%)        <NA>
      3                  6         4 (21%)         3 (23%)        <NA>
      4                  8        12 (63%)         2 (15%)        <NA>
      5               gear            <NA>            <NA>      <0.001
      6                  3        15 (79%)          0 (0%)        <NA>
      7                  4         4 (21%)         8 (62%)        <NA>
      8                  5          0 (0%)         5 (38%)        <NA>
      9                 vs         7 (37%)         7 (54%)         0.3

---

    Code
      as.data.frame(add_p(tbl_summary(trial, by = trt, include = c(grade, response))),
      col_labels = FALSE)
    Output
                 label   stat_1   stat_2 p.value
      1          Grade     <NA>     <NA>     0.9
      2              I 35 (36%) 33 (32%)    <NA>
      3             II 32 (33%) 36 (35%)    <NA>
      4            III 31 (32%) 33 (32%)    <NA>
      5 Tumor Response 28 (29%) 33 (34%)     0.5
      6        Unknown        3        4    <NA>

# add_p.tbl_summary() error messaging with bad inputs

    Code
      add_p(tbl_summary(trial[c("trt", "age")]))
    Condition
      Error in `add_p()`:
      ! Cannot run `add_p()` when `tbl_summary(by)` argument not included.

---

    Code
      add_p(tbl_summary(trial[c("trt", "age")], by = trt), test = list(age = function(
        ...) mtcars))
    Condition
      Error in `add_p()`:
      ! The result from the `test` argument for variable `age` must be an ARD of class <card> or a data frame with one row.
      i Review `?gtsummary::tests()` for details on constructing a custom function.

---

    Code
      add_p(tbl_summary(trial[c("trt", "age")], by = trt), test = list(age = function(
        ...) letters))
    Condition
      Error in `add_p()`:
      ! The result from the `test` argument for variable `age` must be an ARD of class <card> or a data frame with one row.
      i Review `?gtsummary::tests()` for details on constructing a custom function.

# add_p.tbl_summary() & lme4

    Code
      as.data.frame(modify_column_hide(add_p(tbl_summary(trial, by = trt), test = everything() ~
        "lme4", group = response), all_stat_cols()))
    Output
             **Characteristic** **p-value**
      1                     Age        >0.9
      2                 Unknown        <NA>
      3    Marker Level (ng/mL)         0.2
      4                 Unknown        <NA>
      5                 T Stage         0.9
      6                      T1        <NA>
      7                      T2        <NA>
      8                      T3        <NA>
      9                      T4        <NA>
      10                  Grade         0.8
      11                      I        <NA>
      12                     II        <NA>
      13                    III        <NA>
      14         Tumor Response         0.5
      15                Unknown        <NA>
      16           Patient Died         0.3
      17 Months to Death/Censor       0.042

---

    Code
      select(as.data.frame(add_p(tbl_summary(trial, by = trt), test = everything() ~
        "lme4"), col_labels = FALSE), -all_stat_cols())
    Message
      The following errors were returned during `add_p()`:
      x For variable `age` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: The `group` argument cannot be missing for "lme4" tests.
      x For variable `death` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: The `group` argument cannot be missing for "lme4" tests.
      x For variable `grade` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: The `group` argument cannot be missing for "lme4" tests.
      x For variable `marker` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: The `group` argument cannot be missing for "lme4" tests.
      x For variable `response` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: The `group` argument cannot be missing for "lme4" tests.
      x For variable `stage` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: The `group` argument cannot be missing for "lme4" tests.
      x For variable `ttdeath` (`trt`) and "estimate", "std.error", "parameter", "statistic", "conf.low", "conf.high", and "p.value" statistics: The `group` argument cannot be missing for "lme4" tests.
    Output
                          label p.value
      1                     Age    <NA>
      2                 Unknown    <NA>
      3    Marker Level (ng/mL)    <NA>
      4                 Unknown    <NA>
      5                 T Stage    <NA>
      6                      T1    <NA>
      7                      T2    <NA>
      8                      T3    <NA>
      9                      T4    <NA>
      10                  Grade    <NA>
      11                      I    <NA>
      12                     II    <NA>
      13                    III    <NA>
      14         Tumor Response    <NA>
      15                Unknown    <NA>
      16           Patient Died    <NA>
      17 Months to Death/Censor    <NA>

# add_p.tbl_summary() creates output without error/warning for continuous2

    Code
      select(as.data.frame(add_p(tbl_summary(trial, by = grade, include = c(age,
        marker, response), type = all_continuous() ~ "continuous2")), col_labels = FALSE),
      -all_stat_cols())
    Output
                       label p.value
      1                  Age     0.8
      2      Median (Q1, Q3)    <NA>
      3              Unknown    <NA>
      4 Marker Level (ng/mL)   0.019
      5      Median (Q1, Q3)    <NA>
      6              Unknown    <NA>
      7       Tumor Response    >0.9
      8              Unknown    <NA>

# add_p.tbl_summary() works well

    Code
      as.data.frame(add_p(tbl_summary(mtcars, by = am, include = c(mpg, hp, cyl, carb)),
      test = list(mpg = "t.test", hp = "oneway.test", cyl = "chisq.test.no.correct",
        carb = "mood.test")))
    Message
      The following warnings were returned during `add_p()`:
      ! For variable `cyl` (`am`) and "statistic", "p.value", and "parameter" statistics: Chi-squared approximation may be incorrect
    Output
         **Characteristic**   **0**  \nN = 19   **1**  \nN = 13 **p-value**
      1                 mpg 17.3 (14.7, 19.2) 22.8 (21.0, 30.4)       0.001
      2                  hp    175 (110, 205)     109 (66, 113)         0.2
      3                 cyl              <NA>              <NA>       0.013
      4                   4           3 (16%)           8 (62%)        <NA>
      5                   6           4 (21%)           3 (23%)        <NA>
      6                   8          12 (63%)           2 (15%)        <NA>
      7                carb              <NA>              <NA>       0.017
      8                   1           3 (16%)           4 (31%)        <NA>
      9                   2           6 (32%)           4 (31%)        <NA>
      10                  3           3 (16%)            0 (0%)        <NA>
      11                  4           7 (37%)           3 (23%)        <NA>
      12                  6            0 (0%)          1 (7.7%)        <NA>
      13                  8            0 (0%)          1 (7.7%)        <NA>

---

    Code
      as.data.frame(add_p(tbl_summary(mtcars, by = am, include = c(mpg, disp)), test = list(
        mpg = t.test, disp = oneway.test)))
    Output
        **Characteristic**   **0**  \nN = 19   **1**  \nN = 13 **p-value**
      1                mpg 17.3 (14.7, 19.2) 22.8 (21.0, 30.4)       0.001
      2               disp    276 (168, 360)     120 (79, 160)      <0.001

# Groups arg and lme4

    Code
      as.data.frame(tbl_groups)
    Output
        **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102 **p-value**
      1                Age          46 (37, 60)           48 (39, 56)         0.8

# add_p.tbl_summary() can be run after add_difference()

    Code
      add_p(add_p(tbl_summary(select(trial, age, trt), by = trt)))
    Condition
      Error in `add_p()`:
      ! Columns "estimate", "statistic", "conf.low", "conf.high", and "p.value" are already present in table (although, some may be hidden), and no new columns were added.
      i Use `tbl |> modify_table_body(\(x) dplyr::select(x, -p.value))` to remove columns and they will be replaced by the new columns from the current call.

---

    Code
      tbl
    Output
        label stat_1 stat_2 estimate    conf.low p.value
      1   Age 47.011 47.449    -0.03 -0.32, 0.25     0.8

