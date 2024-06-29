# inline_text.tbl_summary() messaging

    Code
      inline_text(test_inline2, variable = "age", column = "Pla5cebo")
    Condition
      Error in `inline_text()`:
      ! Error processing `column` argument.
      ! Can't select columns that don't exist. x Column `Pla5cebo` doesn't exist.
      i Select among columns "variable", "var_type", "row_type", "var_label", "label", "stat_1", "stat_2", "Drug A", and "Drug B"

---

    Code
      inline_text(test_inline2, variable = "stage", level = "Tsdfgsdfg1", column = "Drug B")
    Condition
      Error in `inline_text()`:
      ! `level` must be one of "T1", "T2", "T3", or "T4", not "Tsdfgsdfg1".

---

    Code
      inline_text(test_inline2, variable = "st55age", level = "T1", column = "Drug B")
    Condition
      Error in `inline_text()`:
      ! Error processing `variable` argument.
      ! Can't select columns that don't exist. x Column `st55age` doesn't exist.
      i Select among columns "age", "marker", "stage", "grade", "response", "death", and "ttdeath"

# inline_text.tbl_regression() messaging

    Code
      inline_text(test_inline3, variable = "stage", level = "Tsdfgsdfg1")
    Condition
      Error in `inline_text()`:
      ! `level` must be one of "T1", "T2", "T3", or "T4", not "Tsdfgsdfg1".

---

    Code
      inline_text(test_inline3, variable = "st55age")
    Condition
      Error in `inline_text()`:
      ! Error processing `variable` argument.
      ! Can't select columns that don't exist. x Column `st55age` doesn't exist.
      i Select among columns "age" and "stage"

# inline_text.tbl_survfit() messaging

    Code
      inline_text(tbl1, time = 24, column = "stat_2", level = "Drug A")
    Condition
      Error in `inline_text()`:
      ! Specify one and only one of `time`, `prob`, and `column` arguments.

---

    Code
      inline_text(tbl1, time = 10000, level = "Drug A")
    Condition
      Error in `inline_text()`:
      ! The `time` argument must be one of 12 and 24.

---

    Code
      inline_text(tbl2, prob = 0.2, level = "Drug A")
    Condition
      Error in `inline_text()`:
      ! The `prob` argument must be one of 0.5.

# inline_text.tbl_cross() messaging

    Code
      inline_text(tbl_cross, row_level = "Drug A")
    Condition
      Error in `inline_text()`:
      ! The `col_level` argument cannot be missing.

---

    Code
      inline_text(tbl_cross)
    Condition
      Error in `inline_text()`:
      ! The `col_level` argument cannot be missing.

# check for messaging about duplicate variables

    Code
      tbl_stack(list(t1, t1)) %>% inline_text(variable = marker, column = estimate)
    Message
      Variable marker likely not unique in gtsummary table, and the cell(s) you wish to display may not be accessible. This may occur when gtsummary tables with repeated variable names are combined using `tbl_stack()`.
    Output
      [1] "-0.05"

