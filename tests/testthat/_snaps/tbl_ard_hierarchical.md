# tbl_ard_hierarchical(overall_row) errors when overall results are missing

    Code
      tbl_ard_hierarchical(cards = ard, variables = c(AESOC, AETERM), by = TRTA,
      overall_row = TRUE)
    Condition
      Error in `tbl_ard_hierarchical()`:
      ! Argument `overall_row = TRUE` requires overall results in the `cards` object, but they were not found.
      i Use `over_variables = TRUE` in `cards::ard_stack_hierarchical()` or `cards::ard_stack_hierarchical_count()` to calculate them.

# tbl_ard_hierarchical() checks the structure of the ARD

    Code
      tbl_ard_hierarchical(cards = ard, variables = c(AESOC, AETERM))
    Condition
      Error in `tbl_ard_hierarchical()`:
      ! The structure of the `cards` object does not match the `variables` and `by` arguments.
      i Results for variable "AESOC" are expected to be grouped by no variables, but are grouped by "TRTA".

---

    Code
      tbl_ard_hierarchical(cards = ard, variables = c(AETERM, AESOC), by = TRTA)
    Condition
      Error in `tbl_ard_hierarchical()`:
      ! The structure of the `cards` object does not match the `variables` and `by` arguments.
      i Results for variable "AETERM" are expected to be grouped by "TRTA", but are grouped by "TRTA" and "AESOC".

---

    Code
      tbl_ard_hierarchical(cards = cards::ard_stack_hierarchical(data = ADAE_subset,
        variables = c(AESOC, AETERM), by = TRTA, denominator = cards::ADSL, id = USUBJID,
        include = AETERM), variables = c(AESOC, AETERM), by = TRTA)
    Condition
      Error in `tbl_ard_hierarchical()`:
      ! Results for "AESOC" were not found in the `cards` object.
      i Use the `include` argument to select the variables with summary statistics in the ARD.

---

    Code
      tbl_ard_hierarchical(cards = cards::ard_stack_hierarchical(data = ADAE_subset,
        variables = c(AESOC, AETERM), by = TRTA, denominator = cards::ADSL, id = USUBJID,
        over_variables = TRUE), variables = c(..ard_hierarchical_overall.., AETERM),
      by = TRTA)
    Condition
      Error in `tbl_ard_hierarchical()`:
      ! The `variables` argument cannot include a column named "..ard_hierarchical_overall..".

---

    Code
      tbl_ard_hierarchical(cards = cards::ard_stack_hierarchical_count(data = ADAE_subset,
        variables = c(AESOC, AETERM), by = TRTA), variables = c(AESOC, AETERM), by = TRTA)
    Condition
      Error in `tbl_ard_hierarchical()`:
      ! Statistic "p" is not available for variable "AESOC".
      i Select among "n".

