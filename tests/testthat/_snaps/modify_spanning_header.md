# modify_spanning_header(...) dynamic headers work with `tbl_summary()`

    Code
      modify_spanning_header(tbl_summary(trial, include = marker), label = "This is not a valid {element}.")
    Condition
      Error in `modify_spanning_header()`:
      ! There was an error in the `glue::glue()` evaluation of "This is not a valid {element}." for column "label".
      i Run `gtsummary::show_header_names()` for information on values available for glue interpretation.

# modify_spanning_header() messaging with missing level

    Code
      as_gt(remove_spanning_header(modify_spanning_header(modify_spanning_header(
        tbl_summary(trial, by = trt, include = age), all_stat_cols() ~
          "**Treatments**", level = 1), all_stat_cols() ~ "**Treatments**", level = 2),
      columns = everything(), level = 1))
    Condition
      Error in `as_gt()`:
      ! There is an error in the spanning headers structure.
      ! Each spanning header level must be defined, that is, no levels may be skipped.
      i The spanning header for level 1 is not present, but level 2 is present.

---

    Code
      as_gt(remove_spanning_header(remove_spanning_header(modify_spanning_header(
        modify_spanning_header(modify_spanning_header(tbl_summary(trial, by = trt,
          include = age), all_stat_cols() ~ "**Treatments**", level = 1),
        all_stat_cols() ~ "**Treatments**", level = 2), all_stat_cols() ~
          "**Treatments**", level = 3), columns = everything(), level = 1), columns = everything(),
      level = 2))
    Condition
      Error in `as_gt()`:
      ! There is an error in the spanning headers structure.
      ! Each spanning header level must be defined, that is, no levels may be skipped.
      i The spanning headers for levels 1 and 2 are not present, but level 3 is present.

