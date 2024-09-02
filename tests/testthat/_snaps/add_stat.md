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

# add_stat() with curly braces in errors/warnings

    Code
      curly_warning <- (function(x, ...) {
        warning("{curly} warning")
        10
      })
      as.data.frame(add_stat(tbl, fns = ~curly_warning))
    Message
      There was a warning for variable "age"
      ! {curly} warning
      There was a warning for variable "marker"
      ! {curly} warning
    Output
          **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102 add_stat_1
      1                  Age          46 (37, 60)           48 (39, 56)       10.0
      2 Marker Level (ng/mL)    0.84 (0.23, 1.60)     0.52 (0.18, 1.21)       10.0

---

    Code
      curly_error <- (function(x, ...) {
        stop("{curly} error")
      })
      as.data.frame(add_stat(tbl, fns = ~curly_error))
    Message
      There was a error for variable "age"
      x {curly} error
      There was a error for variable "marker"
      x {curly} error
    Output
          **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102 add_stat_1
      1                  Age          46 (37, 60)           48 (39, 56)       <NA>
      2 Marker Level (ng/mL)    0.84 (0.23, 1.60)     0.52 (0.18, 1.21)       <NA>

