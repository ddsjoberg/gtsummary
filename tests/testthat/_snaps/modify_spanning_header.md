# modify_spanning_header(...) dynamic headers work with `tbl_summary()`

    Code
      modify_spanning_header(tbl_summary(trial, include = marker), label = "This is not a valid {element}.")
    Condition
      Error in `modify_spanning_header()`:
      ! There was an error in the `glue::glue()` evaluation of "This is not a valid {element}." for column "label".

