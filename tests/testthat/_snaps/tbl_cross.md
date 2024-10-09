# tbl_cross(data) works

    Code
      as.data.frame(tbl_cross(trial2))
    Output
                                 T1   T2   T3   T4 Unknown Total
      1 Chemotherapy Treatment <NA> <NA> <NA> <NA>    <NA>  <NA>
      2                 Drug A   28   25   22   23       0    98
      3                 Drug B   25   29   21   27       0   102
      4                Unknown    0    0    0    0       1     1
      5                  Total   53   54   43   50       1   201

# tbl_cross(data) errors properly

    Code
      tbl_cross()
    Condition
      Error in `tbl_cross()`:
      ! The `data` argument cannot be missing.

---

    Code
      tbl_cross(data = letters)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'process_selectors' applied to an object of class "character"

---

    Code
      tbl_cross(data = dplyr::tibble())
    Condition
      Error in `tbl_cross()`:
      ! Error processing `col` argument.
      ! Can't select columns past the end. i Location 2 doesn't exist. i There are only 0 columns.
      i Select among columns

---

    Code
      tbl_cross(data = data.frame())
    Condition
      Error in `tbl_cross()`:
      ! Error processing `col` argument.
      ! Can't select columns past the end. i Location 2 doesn't exist. i There are only 0 columns.
      i Select among columns

# tbl_cross(row, col) works

    Code
      as.data.frame(tbl_cross(trial, row = trt, col = grade))
    Output
                                  I   II  III Total
      1 Chemotherapy Treatment <NA> <NA> <NA>  <NA>
      2                 Drug A   35   32   31    98
      3                 Drug B   33   36   33   102
      4                  Total   68   68   64   200

# tbl_cross(row, col) errors properly

    Code
      tbl_cross(trial, row = trt)
    Condition
      Error in `tbl_cross()`:
      ! Specify both `row` and `col` arguments or neither.

---

    Code
      tbl_cross(mutate(trial, ..total.. = 1), row = "..total..", col = trt)
    Condition
      Error in `tbl_cross()`:
      ! Columns selected with `row` and `col` arguments cannot be named "..total..".

---

    Code
      tbl_cross(trial, col = trt)
    Condition
      Error in `tbl_cross()`:
      ! Specify both `row` and `col` arguments or neither.

---

    Code
      tbl_cross(trial, row = trt, col = 1)
    Condition
      Error in `tbl_cross()`:
      ! Error processing `type` argument.
      ! i In argument: `all_of(row)`. Caused by error in `all_of()`: ! Can't subset elements that don't exist. x Element `trt` doesn't exist.
      i Select among columns "..total.."

---

    Code
      tbl_cross(trial, row = NULL, col = grade)
    Condition
      Error in `tbl_cross()`:
      ! The `row` argument must be length 1.

# tbl_cross(label) errors properly

    Code
      tbl_cross(trial2, label = list(trt = letters))
    Condition
      Error in `tbl_cross()`:
      ! Error in argument `label` for column "trt": value must be a string.

---

    Code
      tbl_cross(trial2, label = letters)
    Condition
      Error in `tbl_cross()`:
      ! The `label` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

---

    Code
      tbl_cross(trial2, label = TRUE)
    Condition
      Error in `tbl_cross()`:
      ! The `label` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

---

    Code
      tbl_cross(trial2, label = list(trt = NA))
    Condition
      Error in `tbl_cross()`:
      ! Error in argument `label` for column "trt": value must be a string.

# tbl_cross(margin) works

    Code
      as.data.frame(tbl_cross(trial2, margin = "column"))
    Output
                                 T1   T2   T3   T4 Unknown Total
      1 Chemotherapy Treatment <NA> <NA> <NA> <NA>    <NA>  <NA>
      2                 Drug A   28   25   22   23       0    98
      3                 Drug B   25   29   21   27       0   102
      4                Unknown    0    0    0    0       1     1

# tbl_cross(margin) errors properly

    Code
      tbl_cross(trial2, margin = "columsadasn")
    Condition
      Error in `tbl_cross()`:
      ! `margin` must be one of "column" or "row", not "columsadasn".

---

    Code
      tbl_cross(trial2, margin = 1)
    Condition
      Error in `tbl_cross()`:
      ! `margin` must be a character vector, not the number 1.

# tbl_cross(percent) errors properly

    Code
      tbl_cross(trial2, percent = "columsadasn")
    Condition
      Error in `tbl_cross()`:
      ! `percent` must be one of "none", "column", "row", or "cell", not "columsadasn".

---

    Code
      tbl_cross(trial2, percent = 1)
    Condition
      Error in `tbl_cross()`:
      ! `percent` must be a character vector, not the number 1.

# tbl_cross(digits) works

    Code
      as.data.frame(tbl_cross(dplyr::bind_rows(rep(list(trial), 11L)), row = grade,
      col = trt, statistic = "{n}/{N_nonmiss}/{N} ({p}%)", digits = c(0, 0, 0, 4)))
    Output
                                    Drug A                       Drug B
      1 grade                         <NA>                         <NA>
      2     I   385/1,078/2,200 (17.5000%)   363/1,122/2,200 (16.5000%)
      3    II   352/1,078/2,200 (16.0000%)   396/1,122/2,200 (18.0000%)
      4   III   341/1,078/2,200 (15.5000%)   363/1,122/2,200 (16.5000%)
      5 Total 1,078/1,078/2,200 (49.0000%) 1,122/1,122/2,200 (51.0000%)
                                Total
      1                          <NA>
      2    748/2,200/2,200 (34.0000%)
      3    748/2,200/2,200 (34.0000%)
      4    704/2,200/2,200 (32.0000%)
      5 2,200/2,200/2,200 (100.0000%)

