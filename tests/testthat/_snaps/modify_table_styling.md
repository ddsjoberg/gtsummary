# modify_table_styling(indent) messaging

    Code
      modify_table_styling(tbl_summary(trial, include = grade), columns = "label",
      rows = !row_type %in% "label", indent = "not an integer")
    Condition
      Error in `modify_table_styling()`:
      ! The `indent` argument must be a non-negative scalar integer.

# modify_table_styling(rows) messaging

    Code
      modify_table_styling(tbl_summary(trial, include = grade), columns = "label",
      rows = "not_a_predicate", indent = 8L)
    Condition
      Error in `modify_table_styling()`:
      ! The `rows` argument must be an expression that evaluates to a logical vector in `x$table_body`.

