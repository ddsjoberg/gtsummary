# modify_footnote(...) dynamic headers work with `tbl_summary()`

    Code
      modify_footnote(tbl_summary(trial, include = marker), label = "This is not a valid {element}.")
    Condition
      Error in `modify_footnote()`:
      ! There was an error in the `glue::glue()` evaluation of "This is not a valid {element}." for column "label".
      i Run `gtsummary::show_header_names()` for information on values available for glue interpretation.

