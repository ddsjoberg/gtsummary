# tbl_split_by_columns() warns if not all columns are selected

    Code
      tbl <- tbl_split_by_columns(tbl_summary(trial, by = trt), groups = list(
        "stat_2"))
    Message
      The following columns were not listed in either `keys` or `groups` argument: "stat_1"
      i These columns have been added to the end of `groups`.
      * Run `gtsummary::show_header_names()` for a list of all column names.

# tbl_split_by_rows(row_numbers) throws errors

    Code
      tbl_lst <- tbl_split_by_rows(tbl_summary(trial, by = trt), row_numbers = -1)
    Condition
      Error in `tbl_split_by_rows()`:
      ! Argument `row_numbers` is out of bounds.
      i Must be between 1 and 17.

---

    Code
      tbl_lst <- tbl_split_by_rows(tbl_summary(trial, by = trt), row_numbers = "grade")
    Condition
      Error in `tbl_split_by_rows()`:
      ! The `row_numbers` argument must an integer vector or empty.

---

    Code
      tbl_split_by_rows(as_gtsummary(mtcars), variables = 1L)
    Condition
      Error in `tbl_split_by_rows()`:
      ! The `variables` argument cannot be specified when `x$table_body` does not have a column named "variable".

# tbl_split_by_rows(row_numbers, variables) throws an error

    Code
      tbl_lst <- tbl_split_by_rows(tbl_summary(trial, by = trt), row_numbers = c(2),
      variables = grade)
    Condition
      Error in `tbl_split_by_rows()`:
      ! One and only one of the following arguments may be specified: `variables`, `row_numbers`, and `variable_level`

# tbl_split_by_rows(variable_level) messaging

    Code
      tbl_split_by_rows(tbl_summary(trial, include = c(age, marker), by = trt,
      missing = "no"), variable_level = all_stat_cols())
    Condition
      Error in `tbl_split_by_rows()`:
      ! The `variable_level` argument may only select a single column when specified.

# nonsensical chained splits error clearly

    Code
      tbl_split_by_rows(tbl_split_by_rows(base_tbl(), variables = c(marker)),
      row_numbers = 2)
    Condition
      Error in `tbl_split_by_rows()`:
      ! The `row_numbers` argument cannot be used after a split by rows.
      i The resulting tables have different numbers of rows, so row positions are ambiguous.
      i Split by `row_numbers` first, or use `variables` or `variable_level` here instead.

---

    Code
      tbl_split_by_rows(tbl_split_by_rows(base_tbl(), variables = c(marker)),
      variables = c(grade))
    Condition
      Error in `tbl_split_by_rows()`:
      ! The `variables` argument cannot be used after a split by rows.
      i Each resulting table holds only a subset of the variables, so a variable split cannot apply across all of them.
      i Split by `variables` first, or use `variable_level` here instead.

---

    Code
      tbl_split_by_rows(tbl_split_by_columns(base_tbl(), groups = list("stat_1",
        "stat_2")))
    Condition
      Error in `tbl_split_by_rows()`:
      ! One and only one of the following arguments may be specified: `variables`, `row_numbers`, and `variable_level`.

---

    Code
      tbl_split_by_rows(tbl_split_by_columns(base_tbl(), groups = list("stat_1",
        "stat_2")), variables = c(marker), row_numbers = 2)
    Condition
      Error in `tbl_split_by_rows()`:
      ! One and only one of the following arguments may be specified: `variables`, `row_numbers`, and `variable_level`.

