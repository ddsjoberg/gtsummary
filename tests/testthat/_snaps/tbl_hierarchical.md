# tbl_hierarchical(data) works properly

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = trt, denominator = trial2,
        id = id))
    Output
        **Chemotherapy Treatment** **N = 200**
      1                     Drug A   45 (45.9)
      2                     Drug B   44 (43.1)

---

    Code
      tbl_hierarchical()
    Condition
      Error in `tbl_hierarchical()`:
      ! The `data` argument cannot be missing.

---

    Code
      tbl_hierarchical(data = letters)
    Condition
      Error in `tbl_hierarchical()`:
      ! The `data` argument must be class <data.frame>, not a character vector.

# tbl_hierarchical(by) works properly

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = stage, by = trt,
        denominator = trial2, id = id))
    Condition
      Warning:
      Duplicate rows found in data for the "id" column.
      i Percentages/Denominators are not correct.
    Output
        **T Stage** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1          T1            22 (22.4)             22 (21.6)
      2          T2            20 (20.4)             22 (21.6)
      3          T3            20 (20.4)             18 (17.6)
      4          T4            21 (21.4)             21 (20.6)

---

    Code
      tbl_hierarchical(data = trial2, variables = stage, by = name, denominator = trial2,
        id = id)
    Condition
      Error in `tbl_hierarchical()`:
      ! Error processing `by` argument.
      ! Can't select columns that don't exist. x Column `name` doesn't exist.
      i Select among columns "trt", "age", "marker", "stage", "grade", "response", "death", "ttdeath", and "id"

# tbl_hierarchical(id) works properly

    Code
      tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = 10)
    Condition
      Error in `tbl_hierarchical()`:
      ! Error processing `id` argument.
      ! Can't select columns past the end. i Location 10 doesn't exist. i There are only 9 columns.
      i Select among columns "trt", "age", "marker", "stage", "grade", "response", "death", "ttdeath", and "id"

# tbl_hierarchical(denominator) works properly

    Code
      tbl_hierarchical(data = trial2, variables = trt, denominator = "test")
    Condition
      Error in `tbl_hierarchical()`:
      ! The `denominator` argument must be a <data.frame> or an <integer>, not a string.

# tbl_hierarchical(include) works properly

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = c(stage, grade),
      denominator = trial2, id = id, include = grade))
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      There were 2 warnings in `dplyr::arrange()`.
      The first warning was:
      i In argument: `..1 = across(c(ord, .data$var_label))`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(ord)
      
        # Now:
        data %>% select(all_of(ord))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Output
         **T Stage**  \n    **Grade** **N = 200**
      1                            T1        <NA>
      2                             I   14 (82.4)
      3                            II   20 (87.0)
      4                           III   12 (92.3)
      5                            T2        <NA>
      6                             I   16 (88.9)
      7                            II   15 (88.2)
      8                           III   17 (89.5)
      9                            T3        <NA>
      10                            I   16 (88.9)
      11                           II    9 (81.8)
      12                          III  14 (100.0)
      13                           T4        <NA>
      14                            I   14 (93.3)
      15                           II   14 (82.4)
      16                          III  18 (100.0)

---

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = c(stage, grade), by = trt,
      denominator = trial2, id = id, include = NULL))
    Condition
      Warning:
      Duplicate rows found in data for the "id" column.
      i Percentages/Denominators are not correct.
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      There were 2 warnings in `dplyr::arrange()`.
      The first warning was:
      i In argument: `..1 = across(c(ord, .data$var_label))`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(ord)
      
        # Now:
        data %>% select(all_of(ord))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Output
         **T Stage**  \n    **Grade** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1                            T1                 <NA>                  <NA>
      2                             I              7 (7.1)               8 (7.8)
      3                            II            12 (12.2)               9 (8.8)
      4                           III              6 (6.1)               6 (5.9)
      5                            T2                 <NA>                  <NA>
      6                             I              7 (7.1)              10 (9.8)
      7                            II              8 (8.2)               7 (6.9)
      8                           III              8 (8.2)               9 (8.8)
      9                            T3                 <NA>                  <NA>
      10                            I            10 (10.2)               7 (6.9)
      11                           II              5 (5.1)               6 (5.9)
      12                          III              6 (6.1)               8 (7.8)
      13                           T4                 <NA>                  <NA>
      14                            I              8 (8.2)               7 (6.9)
      15                           II              5 (5.1)              10 (9.8)
      16                          III            10 (10.2)               8 (7.8)

---

    Code
      tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2,
      id = id, include = name)
    Condition
      Error in `tbl_hierarchical()`:
      ! Error processing `include` argument.
      ! Can't select columns that don't exist. x Column `name` doesn't exist.
      i Select among columns "stage" and "grade"

# tbl_hierarchical(statistic) works properly

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = c(stage, grade),
      denominator = trial2, id = id, statistic = "{n}, {N}, {p}"))
    Output
         **T Stage**  \n    **Grade**   **N = 200**
      1                            T1  36, 53, 67.9
      2                             I  14, 17, 82.4
      3                            II  20, 23, 87.0
      4                           III  12, 13, 92.3
      5                            T2  37, 54, 68.5
      6                             I  16, 18, 88.9
      7                            II  15, 17, 88.2
      8                           III  17, 19, 89.5
      9                            T3  32, 43, 74.4
      10                            I  16, 18, 88.9
      11                           II   9, 11, 81.8
      12                          III 14, 14, 100.0
      13                           T4  35, 50, 70.0
      14                            I  14, 15, 93.3
      15                           II  14, 17, 82.4
      16                          III 18, 18, 100.0

---

    Code
      tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2,
      id = id, statistic = list(stage = "{n}"))
    Condition
      Error in `tbl_hierarchical()`:
      ! The `statistic` argument must be a string, not a list.

# tbl_hierarchical(overall_row) works properly

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = trt, denominator = trial2,
        id = id, overall_row = TRUE))
    Output
                     **Chemotherapy Treatment** **N = 200**
      1 Total number of patients with any event   50 (25.0)
      2                                  Drug A   45 (45.9)
      3                                  Drug B   44 (43.1)

---

    Code
      as.data.frame(res)
    Output
                     **Chemotherapy Treatment** **I**  \nN = 68 **II**  \nN = 68
      1 Total number of patients with any event       40 (58.8)        38 (55.9)
      2                                  Drug A       25 (36.8)        27 (39.7)
      3                                  Drug B       24 (35.3)        26 (38.2)
        **III**  \nN = 64
      1         39 (60.9)
      2         23 (35.9)
      3         26 (40.6)

---

    Code
      tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = id,
        overall_row = "test")
    Condition
      Error in `tbl_hierarchical()`:
      ! The `overall_row` argument must be class <logical>, not a string.

# tbl_hierarchical(label) works properly

    Code
      as.data.frame(res)
    Output
         **My Stage**  \n    **My Grade** **N = 200**
      1                                T1   36 (67.9)
      2                                 I   14 (82.4)
      3                                II   20 (87.0)
      4                               III   12 (92.3)
      5                                T2   37 (68.5)
      6                                 I   16 (88.9)
      7                                II   15 (88.2)
      8                               III   17 (89.5)
      9                                T3   32 (74.4)
      10                                I   16 (88.9)
      11                               II    9 (81.8)
      12                              III  14 (100.0)
      13                               T4   35 (70.0)
      14                                I   14 (93.3)
      15                               II   14 (82.4)
      16                              III  18 (100.0)

---

    "**My Stage**  \n    **My Grade**"

---

    Code
      tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2,
      id = id, label = "Stages")
    Condition
      Error in `internal_tbl_hierarchical()`:
      ! The `label` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

# tbl_hierarchical works properly when last variable of hierarchy is ordered

    Code
      as.data.frame(res_o)
    Output
         **T Stage**  \n    **grade** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1                            T1            22 (22.4)             22 (21.6)
      2                             I              4 (4.1)               7 (6.9)
      3                            II            12 (12.2)               9 (8.8)
      4                           III              6 (6.1)               6 (5.9)
      5                            T2            20 (20.4)             22 (21.6)
      6                             I              5 (5.1)               7 (6.9)
      7                            II              7 (7.1)               6 (5.9)
      8                           III              8 (8.2)               9 (8.8)
      9                            T3            20 (20.4)             18 (17.6)
      10                            I            10 (10.2)               5 (4.9)
      11                           II              4 (4.1)               5 (4.9)
      12                          III              6 (6.1)               8 (7.8)
      13                           T4            21 (21.4)             21 (20.6)
      14                            I              6 (6.1)               4 (3.9)
      15                           II              5 (5.1)               9 (8.8)
      16                          III            10 (10.2)               8 (7.8)

---

    Code
      as.data.frame(res)
    Output
         **T Stage**  \n    **grade** **N = 200**
      1                            T1   36 (67.9)
      2                             I   14 (82.4)
      3                            II   20 (87.0)
      4                           III   12 (92.3)
      5                            T2   37 (68.5)
      6                             I   16 (88.9)
      7                            II   15 (88.2)
      8                           III   17 (89.5)
      9                            T3   32 (74.4)
      10                            I   16 (88.9)
      11                           II    9 (81.8)
      12                          III  14 (100.0)
      13                           T4   35 (70.0)
      14                            I   14 (93.3)
      15                           II   14 (82.4)
      16                          III  18 (100.0)

# tbl_hierarchical_count(data) works properly

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, variables = trt))
    Output
        **Chemotherapy Treatment** Overall
      1                     Drug A      98
      2                     Drug B     102

---

    Code
      as.data.frame(tbl_hierarchical_count(data = iris, variables = Species))
    Output
        **Species** Overall
      1      setosa      50
      2  versicolor      50
      3   virginica      50

---

    Code
      tbl_hierarchical_count()
    Condition
      Error in `tbl_hierarchical_count()`:
      ! The `data` argument cannot be missing.

---

    Code
      tbl_hierarchical_count(data = letters)
    Condition
      Error in `tbl_hierarchical_count()`:
      ! The `data` argument must be class <data.frame>, not a character vector.

# tbl_hierarchical_count(by) works properly

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, variables = stage, by = trt))
    Output
        **T Stage** **Drug A** **Drug B**
      1          T1         28         25
      2          T2         25         29
      3          T3         22         21
      4          T4         23         27

---

    Code
      tbl_hierarchical_count(data = trial, variables = stage, by = name)
    Condition
      Error in `tbl_hierarchical_count()`:
      ! Error processing `by` argument.
      ! Can't select columns that don't exist. x Column `name` doesn't exist.
      i Select among columns "trt", "age", "marker", "stage", "grade", "response", "death", and "ttdeath"

# tbl_hierarchical_count(denominator) works properly

    Code
      as.data.frame(res)
    Output
        **Chemotherapy Treatment** **N = 400**
      1                     Drug A          98
      2                     Drug B         102

---

    Code
      tbl_hierarchical_count(data = trial, variables = trt, denominator = "test")
    Condition
      Error in `tbl_hierarchical_count()`:
      ! The `denominator` argument must be empty, a <data.frame>, or an <integer>, not a string.

# tbl_hierarchical_count(include) works properly

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, variables = c(stage, grade),
      include = grade))
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      There were 2 warnings in `dplyr::arrange()`.
      The first warning was:
      i In argument: `..1 = across(c(ord, .data$var_label))`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(ord)
      
        # Now:
        data %>% select(all_of(ord))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Output
         **T Stage**  \n    **Grade** Overall
      1                            T1    <NA>
      2                             I      17
      3                            II      23
      4                           III      13
      5                            T2    <NA>
      6                             I      18
      7                            II      17
      8                           III      19
      9                            T3    <NA>
      10                            I      18
      11                           II      11
      12                          III      14
      13                           T4    <NA>
      14                            I      15
      15                           II      17
      16                          III      18

---

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, variables = c(stage, grade),
      by = trt, include = NULL))
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`
      Warning:
      There were 2 warnings in `dplyr::arrange()`.
      The first warning was:
      i In argument: `..1 = across(c(ord, .data$var_label))`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(ord)
      
        # Now:
        data %>% select(all_of(ord))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Output
         **T Stage**  \n    **Grade** **Drug A** **Drug B**
      1                            T1       <NA>       <NA>
      2                             I          8          9
      3                            II         14          9
      4                           III          6          7
      5                            T2       <NA>       <NA>
      6                             I          8         10
      7                            II          8          9
      8                           III          9         10
      9                            T3       <NA>       <NA>
      10                            I         11          7
      11                           II          5          6
      12                          III          6          8
      13                           T4       <NA>       <NA>
      14                            I          8          7
      15                           II          5         12
      16                          III         10          8

---

    Code
      tbl_hierarchical_count(data = trial, variables = c(stage, grade), include = name)
    Condition
      Error in `tbl_hierarchical_count()`:
      ! Error processing `include` argument.
      ! Can't select columns that don't exist. x Column `name` doesn't exist.
      i Select among columns "stage" and "grade"

# tbl_hierarchical_count(overall_row) works properly

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, variables = trt,
        overall_row = TRUE))
    Output
        **Chemotherapy Treatment** Overall
      1    Total number of records     200
      2                     Drug A      98
      3                     Drug B     102

---

    Code
      as.data.frame(res)
    Output
        **Chemotherapy Treatment** **I** **II** **III**
      1    Total number of records    68     68      64
      2                     Drug A    35     32      31
      3                     Drug B    33     36      33

---

    Code
      tbl_hierarchical_count(data = trial, variables = trt, overall_row = "test")
    Condition
      Error in `tbl_hierarchical_count()`:
      ! The `overall_row` argument must be class <logical>, not a string.

# tbl_hierarchical_count(label) works properly

    Code
      as.data.frame(res)
    Output
         **My Stage**  \n    **My Grade** Overall
      1                                T1      53
      2                                 I      17
      3                                II      23
      4                               III      13
      5                                T2      54
      6                                 I      18
      7                                II      17
      8                               III      19
      9                                T3      43
      10                                I      18
      11                               II      11
      12                              III      14
      13                               T4      50
      14                                I      15
      15                               II      17
      16                              III      18

---

    "**My Stage**  \n    **My Grade**"

---

    Code
      tbl_hierarchical_count(data = trial, variables = c(stage, grade), label = "Stages")
    Condition
      Error in `internal_tbl_hierarchical()`:
      ! The `label` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

