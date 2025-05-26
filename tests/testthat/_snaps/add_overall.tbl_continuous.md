# add_overall.tbl_continuous() messaging

    Code
      tbl <- add_overall(tbl_continuous(mtcars, include = gear, variable = mpg))
    Message
      Cannot add an overall column with `add_overall()` when original table is not statified with `tbl_continuous(by)`.
      i Returning table unaltered.

