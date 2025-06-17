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

# tbl_split_by_rows(row_numbers, variables) throws an error

    Code
      tbl_lst <- tbl_split_by_rows(tbl_summary(trial, by = trt), row_numbers = c(2),
      variables = grade)
    Condition
      Error in `tbl_split_by_rows()`:
      ! Please select only one and only one between `row_numbers` and `variables` arguments.

