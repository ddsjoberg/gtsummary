# tbl_split_by_columns() warns if not all columns are selected

    Code
      tbl <- tbl_split_by_columns(tbl_summary(trial, by = trt), groups = list(
        "stat_2"))
    Message
      The following columns were not listed in either `keys` or `groups` argument: "stat_1"
      i These columns have been added to the end of `groups`.
      * Run `gtsummary::show_header_names()` for a list of all column names.

