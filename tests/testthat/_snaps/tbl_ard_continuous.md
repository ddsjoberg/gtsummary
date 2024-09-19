# tbl_ard_continuous(cards)

    Code
      as.data.frame(tbl_ard_continuous(cards::ard_continuous(trial, by = grade,
        variables = age), variable = "age", include = "grade"))
    Output
        **Characteristic**       **Overall**
      1              grade              <NA>
      2                  I 47.0 (37.0, 56.0)
      3                 II 48.5 (37.0, 57.0)
      4                III 47.0 (38.0, 58.0)

---

    Code
      as.data.frame(tbl_ard_continuous(cards::bind_ard(cards::ard_continuous(trial,
        by = c(trt, grade), variables = age), cards::ard_categorical(trial, trt)),
      variable = "age", include = "grade", by = "trt"))
    Output
        **Characteristic**        **Drug A**        **Drug B**
      1              grade              <NA>              <NA>
      2                  I 46.0 (36.0, 60.0) 48.0 (42.0, 55.0)
      3                 II 44.5 (31.0, 55.0) 50.5 (42.0, 57.5)
      4                III 51.5 (41.5, 60.5) 45.0 (36.0, 52.0)

---

    Code
      as.data.frame(tbl_ard_continuous(cards::ard_continuous(trial, by = c(trt, grade),
      variables = age), by = trt, variable = age, include = grade))
    Output
        **Characteristic**        **Drug A**        **Drug B**
      1              grade              <NA>              <NA>
      2                  I 46.0 (36.0, 60.0) 48.0 (42.0, 55.0)
      3                 II 44.5 (31.0, 55.0) 50.5 (42.0, 57.5)
      4                III 51.5 (41.5, 60.5) 45.0 (36.0, 52.0)

# tbl_ard_continuous(cards) error messaging

    Code
      tbl_ard_continuous(cards::bind_ard(cards::ard_continuous(trial, by = c(trt,
        grade), variables = age)), variable = "trt", include = "grade", by = "age")
    Condition
      Error in `tbl_ard_continuous()`:
      ! The continuous variable specified in argument `variable` must appear in the column `cards$variable`.

---

    Code
      tbl_ard_continuous(cards::bind_ard(cards::ard_continuous(trial, by = c(trt,
        grade), variables = age)), variable = "age", include = "trt", by = "grade")
    Condition
      Error in `tbl_ard_continuous()`:
      ! All variables specified in argument `include` must appear in the column `cards$group2`.

# tbl_ard_continuous(statistic) error messaging

    Code
      tbl_ard_continuous(cards::ard_continuous(trial, by = grade, variables = age),
      variable = "age", include = "grade", statistic = everything() ~ c("{mean}",
        "{median}"))
    Condition
      Error in `tbl_ard_continuous()`:
      ! Elements of the `statistic` argument must be strings.

# tbl_ard_continuous(value) messaging

    Code
      tbl_ard_continuous(cards::ard_continuous(trial, by = c(trt, grade), variables = age),
      by = "trt", variable = "age", include = "grade", value = grade ~ "XXXXXXX")
    Condition
      Error in `tbl_ard_continuous()`:
      ! There was an error in the `value` argument for variable "grade".
      The list value must be one of I, II, and III.

---

    Code
      tbl_ard_continuous(cards::ard_continuous(trial, by = c(trt, grade), variables = age),
      by = "trt", variable = "age", include = "grade", value = grade ~ letters)
    Condition
      Error in `tbl_ard_continuous()`:
      ! Error in argument `value` for variable "grade".
      i Elements values must be a scalar.

