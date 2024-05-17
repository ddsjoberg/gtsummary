# add_stat(fns) messaging

    Code
      result <- add_stat(tbl, fns = everything() ~ mean)
    Message
      There was a error for variable "age"
      x argument "x" is missing, with no default
      There was a error for variable "marker"
      x argument "x" is missing, with no default

---

    Code
      add_stat(tbl_summary(trial, include = age, type = age ~ "continuous2",
      statistic = everything() ~ c("{mean}", "{sd}")), fns = everything() ~
        return_two_by_two_10s, location = everything() ~ "label")
    Condition
      Error in `add_stat()`:
      ! Dimension of "age" and the added statistic do not match.
      i Expecting statistic/data frame to be length/no. rows 1.

---

    Code
      add_stat(add_stat(tbl_summary(trial, include = age, type = age ~ "continuous2",
      statistic = everything() ~ c("{mean}", "{sd}")), fns = everything() ~
        return_two_by_two_10s, location = everything() ~ "level"), fns = everything() ~
        return_two_by_two_10s, location = everything() ~ "level")
    Condition
      Error in `add_stat()`:
      ! Cannot add new column that already exist in <gtsummary> table: "one" and "two".

# add_stat(x) messaging

    Code
      add_stat(mtcars, fns = everything() ~ my_ttest2)
    Condition
      Error in `add_stat()`:
      ! The `x` argument must be class <tbl_summary/tbl_svysummary/tbl_continuous>, not a data frame.

