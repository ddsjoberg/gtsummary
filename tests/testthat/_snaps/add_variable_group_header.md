# add_variable_group_header() messaging

    Code
      add_variable_group_header(modify_table_body(tbl_summary(trial, include = c(age,
        response, death, marker), missing = "no"), ~ dplyr::relocate(.x, label,
        .after = "stat_0")), header = "the header row", variables = c(response, death))
    Condition
      Error in `add_variable_group_header()`:
      ! The <gtsummary> table must include columns "variable", "row_type", and "label" in the `x$table_body` data frame and the "label" column must appear first.

