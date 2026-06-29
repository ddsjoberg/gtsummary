# remove_abbreviation() messaging

    Code
      remove_abbreviation(tbl_summary(trial, include = marker), "Q3 = Third Quartile")
    Condition
      Error in `remove_abbreviation()`:
      ! There are no abbreviations to remove.

---

    Code
      remove_abbreviation(modify_abbreviation(tbl_summary(trial, include = marker),
      "Q1 = First Quartile"), "Q3 = Third Quartile")
    Condition
      Error in `remove_abbreviation()`:
      ! The `abbreviation` argument must be one of "Q1 = First Quartile".

# modify_abbreviation(prefix, sep) input checks

    Code
      modify_abbreviation(tbl_summary(trial, include = marker), "Q1 = First Quartile",
      prefix = "one")
    Condition
      Error in `modify_abbreviation()`:
      ! The `prefix` argument must be a character vector of length 2.

---

    Code
      modify_abbreviation(tbl_summary(trial, include = marker), "Q1 = First Quartile",
      sep = 1)
    Condition
      Error in `modify_abbreviation()`:
      ! The `sep` argument must be a string, not a number.

