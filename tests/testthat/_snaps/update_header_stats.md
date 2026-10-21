# update_header_stats() messaging

    Code
      update_header_stats(tbl_by, card = trial)
    Condition
      Error in `update_header_stats()`:
      ! The `card` argument must be class <card>, not a tibble.

---

    Code
      update_header_stats(tbl_by, card = cards::ard_tabulate(trial, by = grade,
        variables = trt))
    Condition
      Error in `update_header_stats()`:
      ! The `card` argument object cannot include group columns, i.e. "group1" and "group1_level".

---

    Code
      update_header_stats(tbl_by, card = cards::ard_tabulate(trial, variables = c(trt,
        grade)))
    Condition
      Error in `update_header_stats()`:
      ! The `card` argument object must contain results for a single variable.

---

    Code
      update_header_stats(tbl_by, card = cards::ard_tabulate(trial, variables = grade))
    Condition
      Error in `update_header_stats()`:
      ! The number of levels in the `card` object (I, II, and III) must match the number of statistic columns "stat_1" and "stat_2".

---

    Code
      update_header_stats(tbl_by, card = cards::ard_total_n(trial))
    Condition
      Error in `update_header_stats()`:
      ! For tables with a `by` variable (i.e. columns "stat_1" and "stat_2"), the `card` argument must be a tabulation of a single variable.
      i This object is typically created with `cards::ard_tabulate()`.

---

    Code
      update_header_stats(tbl_overall, card = cards::ard_tabulate(trial, variables = trt))
    Condition
      Error in `update_header_stats()`:
      ! For tables without a `by` variable (i.e. a single "stat_0" column), the `card` argument must contain the total N.
      i This object is typically created with `cards::ard_total_n()`.

---

    Code
      update_header_stats(tbl_regression(lm(age ~ trt, trial)), card = cards::ard_total_n(
        trial))
    Condition
      Error in `update_header_stats()`:
      ! The `x` table must contain an overall "stat_0" column and/or sequential stat columns, e.g. "stat_1", "stat_2", and "stat_3".
      i Run `show_header_names()` to see column names.

