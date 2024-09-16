# add_stat_label(location='row') standard use

    Code
      as.data.frame(modify_column_hide(add_stat_label(tbl_ard, location = "row"),
      all_stat_cols()))
    Output
          **Characteristic**
      1        AGEGR1, n (%)
      2                  <65
      3                65-80
      4                  >80
      5 Age, Median (Q1, Q3)

# add_stat_label(location='column') standard use

    Code
      as.data.frame(modify_column_hide(add_stat_label(tbl_ard, location = "column"),
      all_stat_cols()))
    Output
        **Characteristic**   **Statistic**
      1             AGEGR1            <NA>
      2                <65           n (%)
      3              65-80           n (%)
      4                >80           n (%)
      5                Age Median (Q1, Q3)

---

    Code
      as.data.frame(modify_column_hide(add_stat_label(tbl_ard, location = "column",
        label = all_categorical() ~ "no. (%)"), all_stat_cols()))
    Output
        **Characteristic**   **Statistic**
      1             AGEGR1            <NA>
      2                <65         no. (%)
      3              65-80         no. (%)
      4                >80         no. (%)
      5                Age Median (Q1, Q3)

# add_stat_label(label) standard use

    Code
      as.data.frame(add_stat_label(tbl_ard_summary(cards::ard_stack(data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"), cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE, .missing = TRUE, .total_n = TRUE), type = all_continuous() ~
        "continuous2", statistic = all_continuous() ~ c("{median} ({p25}, {p75})",
        "{min} - {max}")), label = AGE ~ c("Median (IQR)", "Range")))
    Output
               **Characteristic**       **Overall**
      1 Pooled Age Group 1, n (%)              <NA>
      2                     65-80       144 (56.7%)
      3                       <65        33 (13.0%)
      4                       >80        77 (30.3%)
      5                       Age              <NA>
      6              Median (IQR) 77.0 (70.0, 81.0)
      7                     Range       51.0 - 89.0

# add_stat_label(label) messaging

    Code
      add_stat_label(tbl_ard_summary(cards::ard_stack(data = cards::ADSL, cards::ard_categorical(
        variables = "AGEGR1"), cards::ard_continuous(variables = "AGE"), .attributes = TRUE,
      .missing = TRUE, .total_n = TRUE), type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}")),
      label = AGE ~ letters)
    Condition
      Error in `add_stat_label()`:
      ! The element of the `label` argument for variable "AGE" must be a string of length 2.

---

    Code
      add_stat_label(tbl_ard_summary(cards::ard_stack(data = cards::ADSL, cards::ard_categorical(
        variables = "AGEGR1"), cards::ard_continuous(variables = "AGE"), .attributes = TRUE,
      .missing = TRUE, .total_n = TRUE), type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}")),
      label = AGE ~ c("Median (IQR)", "Range", "TOO LONG!"))
    Condition
      Error in `add_stat_label()`:
      ! The element of the `label` argument for variable "AGE" must be a string of length 2.

# add_stat_label() messaging

    Code
      invisible(add_stat_label(add_stat_label(tbl_ard_summary(cards::ard_stack(data = cards::ADSL,
      cards::ard_categorical(variables = "AGEGR1"), cards::ard_continuous(variables = "AGE"),
      .attributes = TRUE, .missing = TRUE, .total_n = TRUE), type = all_continuous() ~
        "continuous2", statistic = all_continuous() ~ c("{median} ({p25}, {p75})",
        "{min} - {max}")))))
    Message
      `add_stat_label()` has previously been applied. Returning gtsummary table unaltered.

