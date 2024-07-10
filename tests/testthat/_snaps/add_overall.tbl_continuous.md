# add_overall.tbl_continuous() errors

    Code
      add_overall(tbl_continuous(mtcars, include = gear, variable = mpg))
    Condition
      Error in `add_overall()`:
      ! Cannot run `add_overall()` when original table function is not statified with `tbl_continuous(by)`.

