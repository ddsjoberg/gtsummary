# add_stat_label(location='row') standard use

    Code
      as.data.frame(modify_column_hide(add_stat_label(tbl, location = "row"),
      all_stat_cols()))
    Output
                              **Characteristic**
      1                     Age, Median (Q1, Q3)
      2                                  Unknown
      3    Marker Level (ng/mL), Median (Q1, Q3)
      4                                  Unknown
      5                           T Stage, n (%)
      6                                       T1
      7                                       T2
      8                                       T3
      9                                       T4
      10                            Grade, n (%)
      11                                       I
      12                                      II
      13                                     III
      14                   Tumor Response, n (%)
      15                                 Unknown
      16                     Patient Died, n (%)
      17 Months to Death/Censor, Median (Q1, Q3)

# add_stat_label(location='column') standard use

    Code
      as.data.frame(modify_column_hide(add_stat_label(tbl, location = "column"),
      all_stat_cols()))
    Output
             **Characteristic**   **Statistic**
      1                     Age Median (Q1, Q3)
      2                 Unknown               n
      3    Marker Level (ng/mL) Median (Q1, Q3)
      4                 Unknown               n
      5                 T Stage            <NA>
      6                      T1           n (%)
      7                      T2           n (%)
      8                      T3           n (%)
      9                      T4           n (%)
      10                  Grade            <NA>
      11                      I           n (%)
      12                     II           n (%)
      13                    III           n (%)
      14         Tumor Response           n (%)
      15                Unknown               n
      16           Patient Died           n (%)
      17 Months to Death/Censor Median (Q1, Q3)

---

    Code
      as.data.frame(modify_column_hide(add_stat_label(tbl, location = "column",
        label = all_categorical() ~ "no. (%)"), all_stat_cols()))
    Output
             **Characteristic**   **Statistic**
      1                     Age Median (Q1, Q3)
      2                 Unknown               n
      3    Marker Level (ng/mL) Median (Q1, Q3)
      4                 Unknown               n
      5                 T Stage            <NA>
      6                      T1         no. (%)
      7                      T2         no. (%)
      8                      T3         no. (%)
      9                      T4         no. (%)
      10                  Grade            <NA>
      11                      I         no. (%)
      12                     II         no. (%)
      13                    III         no. (%)
      14         Tumor Response         no. (%)
      15                Unknown               n
      16           Patient Died         no. (%)
      17 Months to Death/Censor Median (Q1, Q3)

# add_stat_label(label) standard use

    Code
      as.data.frame(add_stat_label(tbl_svysummary(svy_trial, include = c(age, grade,
        trt), by = trt, type = all_continuous() ~ "continuous2", statistic = all_continuous() ~
        c("{median} ({p25}, {p75})", "{min} - {max}"), ), label = age ~ c(
        "Median (IQR)", "Range")))
    Output
        **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1                Age                 <NA>                  <NA>
      2       Median (IQR)          46 (37, 60)           48 (39, 56)
      3              Range               6 - 78                9 - 83
      4            Unknown                    7                     4
      5       Grade, n (%)                 <NA>                  <NA>
      6                  I             35 (36%)              33 (32%)
      7                 II             32 (33%)              36 (35%)
      8                III             31 (32%)              33 (32%)

# add_stat_label(label) messaging

    Code
      add_stat_label(tbl_svysummary(svy_trial, include = c(age, trt), by = trt, ),
      label = age ~ letters)
    Condition
      Error:
      ! Elements of the `label` argument for variable "age" must be a string of length 1.

---

    Code
      add_stat_label(tbl_svysummary(svy_trial, include = c(age, grade, trt), by = trt,
      type = all_continuous() ~ "continuous2", statistic = all_continuous() ~ c(
        "{median} ({p25}, {p75})", "{min} - {max}"), ), label = age ~ c(
        "Median (IQR)", "Range", "TOO LONG!"))
    Condition
      Error in `add_stat_label()`:
      ! The element of the `label` argument for variable "age" must be a string of length 2.

# add_stat_label() messaging

    Code
      invisible(add_stat_label(add_stat_label(tbl_svysummary(svy_trial, include = c(
        age, trt), ))))
    Message
      `add_stat_label()` has previously been applied. Returning gtsummary table unaltered.

