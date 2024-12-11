# modify_footnote_body(rows) messaging

    Code
      modify_footnote_body(base_tbl_summary, footnote = "this will not appear",
        columns = label, rows = not_a_predicate)
    Condition
      Error in `modify_footnote_body()`:
      ! The `rows` argument must be an expression that evaluates to a logical vector in `x$table_body`.

