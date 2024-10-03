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
      1          T1            22 (78.6)             22 (88.0)
      2          T2            20 (80.0)             22 (75.9)
      3          T3            20 (90.9)             18 (85.7)
      4          T4            21 (91.3)             21 (77.8)

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
    Output
         **T Stage**  \n    **Grade** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1                            T1                 <NA>                  <NA>
      2                             I             7 (87.5)              8 (88.9)
      3                            II            12 (85.7)             9 (100.0)
      4                           III            6 (100.0)              6 (85.7)
      5                            T2                 <NA>                  <NA>
      6                             I             7 (87.5)            10 (100.0)
      7                            II            8 (100.0)              7 (77.8)
      8                           III             8 (88.9)              9 (90.0)
      9                            T3                 <NA>                  <NA>
      10                            I            10 (90.9)             7 (100.0)
      11                           II            5 (100.0)             6 (100.0)
      12                          III            6 (100.0)             8 (100.0)
      13                           T4                 <NA>                  <NA>
      14                            I            8 (100.0)             7 (100.0)
      15                           II            5 (100.0)             10 (83.3)
      16                          III           10 (100.0)             8 (100.0)

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
      2                                  Drug A       25 (71.4)        27 (84.4)
      3                                  Drug B       24 (72.7)        26 (72.2)
        **III**  \nN = 64
      1         39 (60.9)
      2         23 (74.2)
      3         26 (78.8)

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
         **Primary System Organ Class**  \n    **Highest Severity**
      1                                           CARDIAC DISORDERS
      2                                                        MILD
      3                                                    MODERATE
      4                                                      SEVERE
      5                                  GASTROINTESTINAL DISORDERS
      6                                                        MILD
      7                                                    MODERATE
      8                                                      SEVERE
      9        GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
      10                                                       MILD
      11                                                   MODERATE
      12                                                     SEVERE
      13                                INFECTIONS AND INFESTATIONS
      14                                                       MILD
      15                                                   MODERATE
      16                                                     SEVERE
      17                     SKIN AND SUBCUTANEOUS TISSUE DISORDERS
      18                                                       MILD
      19                                                   MODERATE
      20                                                     SEVERE
         **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84
      1                   <NA>                               <NA>
      2                2 (2.3)                            1 (1.2)
      3                0 (0.0)                            2 (2.4)
      4                1 (1.2)                            0 (0.0)
      5                   <NA>                               <NA>
      6               9 (10.5)                            2 (2.4)
      7                1 (1.2)                            2 (2.4)
      8                0 (0.0)                            0 (0.0)
      9                   <NA>                               <NA>
      10               8 (9.3)                          15 (17.9)
      11               1 (1.2)                          13 (15.5)
      12               0 (0.0)                            0 (0.0)
      13                  <NA>                               <NA>
      14               4 (4.7)                            2 (2.4)
      15               2 (2.3)                            1 (1.2)
      16               0 (0.0)                            0 (0.0)
      17                  <NA>                               <NA>
      18               5 (5.8)                          11 (13.1)
      19               4 (4.7)                            4 (4.8)
      20               0 (0.0)                            0 (0.0)
         **Xanomeline Low Dose**  \nN = 84
      1                               <NA>
      2                            0 (0.0)
      3                            0 (0.0)
      4                            0 (0.0)
      5                               <NA>
      6                            5 (6.0)
      7                            0 (0.0)
      8                            0 (0.0)
      9                               <NA>
      10                         14 (16.7)
      11                         11 (13.1)
      12                           2 (2.4)
      13                              <NA>
      14                           1 (1.2)
      15                           0 (0.0)
      16                           0 (0.0)
      17                              <NA>
      18                           6 (7.1)
      19                          9 (10.7)
      20                           0 (0.0)

---

    Code
      as.data.frame(res)
    Output
              **Primary System Organ Class**  \n    **AESEV** **N = 254**
      1                                     CARDIAC DISORDERS     6 (2.4)
      2                                                  MILD     3 (1.2)
      3                                              MODERATE     2 (0.8)
      4                                                SEVERE     1 (0.4)
      5                            GASTROINTESTINAL DISORDERS    19 (7.5)
      6                                                  MILD    16 (6.3)
      7                                              MODERATE     3 (1.2)
      8                                                SEVERE     0 (0.0)
      9  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS   64 (25.2)
      10                                                 MILD   37 (14.6)
      11                                             MODERATE    25 (9.8)
      12                                               SEVERE     2 (0.8)
      13                          INFECTIONS AND INFESTATIONS    10 (3.9)
      14                                                 MILD     7 (2.8)
      15                                             MODERATE     3 (1.2)
      16                                               SEVERE     0 (0.0)
      17               SKIN AND SUBCUTANEOUS TISSUE DISORDERS   39 (15.4)
      18                                                 MILD    22 (8.7)
      19                                             MODERATE    17 (6.7)
      20                                               SEVERE     0 (0.0)

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
      1     Total number of events     200
      2                     Drug A      98
      3                     Drug B     102

---

    Code
      as.data.frame(res)
    Output
        **Chemotherapy Treatment** **I** **II** **III**
      1     Total number of events    68     68      64
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

