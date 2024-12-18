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

