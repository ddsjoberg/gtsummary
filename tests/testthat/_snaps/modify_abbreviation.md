# modify_abbreviation() accepts a character vector of abbreviations

    Code
      modify_abbreviation(tbl_summary(trial, include = marker), character(0))
    Condition
      Error in `modify_abbreviation()`:
      ! The `abbreviation` argument must specify at least one abbreviation.

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

# modify_abbreviation(prefix, sep1, sep2) input checks

    Code
      modify_abbreviation(tbl_summary(trial, include = marker), "Q1 = First Quartile",
      prefix = "one")
    Condition
      Error in `modify_abbreviation()`:
      ! The `prefix` argument must be a character vector of length 2.

---

    Code
      modify_abbreviation(tbl_summary(trial, include = marker), "Q1 = First Quartile",
      sep1 = 1)
    Condition
      Error in `modify_abbreviation()`:
      ! The `sep1` argument must be a string, not a number.

---

    Code
      modify_abbreviation(tbl_summary(trial, include = marker), "Q1 = First Quartile",
      sep2 = 1)
    Condition
      Error in `modify_abbreviation()`:
      ! The `sep2` argument must be a string, not a number.

