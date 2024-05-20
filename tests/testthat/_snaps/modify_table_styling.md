# modify_table_styling(cols_merge_pattern) messaging

    Code
      modify_table_styling(tbl_regression(lm(mpg ~ factor(am), mtcars)), columns = "label",
      rows = !is.na(conf.low), cols_merge_pattern = "{conf.low}:::{conf.high}")
    Condition
      Error in `modify_table_styling()`:
      ! A single column must be specified in the `columns` argument when using `cols_merge_pattern`, and that column must be the first to appear in the pattern argument.
      i For example, `modify_table_styling(columns="conf.low", cols_merge_pattern="{conf.low}:::{conf.high}")`

---

    Code
      modify_table_styling(tbl_regression(lm(mpg ~ factor(am), mtcars)), columns = "conf.low",
      rows = !is.na(conf.low), cols_merge_pattern = "{conf.low}:::{not_in_table}")
    Condition
      Error in `modify_table_styling()`:
      ! All columns specified in `cols_merge_pattern` argument must be present in `x$table_body`
      i The following columns are not present: "not_in_table"

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

