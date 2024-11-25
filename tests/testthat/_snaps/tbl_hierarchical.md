# tbl_hierarchical(data) works properly

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = trt, denominator = trial2,
        id = id))
    Output
        **Chemotherapy Treatment** **N = 200**
      1                     Drug A    45 (46%)
      2                     Drug B    44 (43%)

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
      1          T1             22 (79%)              22 (88%)
      2          T2             20 (80%)              22 (76%)
      3          T3             20 (91%)              18 (86%)
      4          T4             21 (91%)              21 (78%)

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
      tbl_hierarchical(data = trial2, variables = trt, denominator = "test", id = id)
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
      2                             I    14 (82%)
      3                            II    20 (87%)
      4                           III    12 (92%)
      5                            T2        <NA>
      6                             I    16 (89%)
      7                            II    15 (88%)
      8                           III    17 (89%)
      9                            T3        <NA>
      10                            I    16 (89%)
      11                           II     9 (82%)
      12                          III   14 (100%)
      13                           T4        <NA>
      14                            I    14 (93%)
      15                           II    14 (82%)
      16                          III   18 (100%)

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
      2                             I              7 (88%)               8 (89%)
      3                            II             12 (86%)              9 (100%)
      4                           III             6 (100%)               6 (86%)
      5                            T2                 <NA>                  <NA>
      6                             I              7 (88%)             10 (100%)
      7                            II             8 (100%)               7 (78%)
      8                           III              8 (89%)               9 (90%)
      9                            T3                 <NA>                  <NA>
      10                            I             10 (91%)              7 (100%)
      11                           II             5 (100%)              6 (100%)
      12                          III             6 (100%)              8 (100%)
      13                           T4                 <NA>                  <NA>
      14                            I             8 (100%)              7 (100%)
      15                           II             5 (100%)              10 (83%)
      16                          III            10 (100%)              8 (100%)

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
      denominator = trial2, id = id, statistic = ~"{n}, {N}, {p}"))
    Output
         **T Stage**  \n    **Grade** **N = 200**
      1                            T1  36, 53, 68
      2                             I  14, 17, 82
      3                            II  20, 23, 87
      4                           III  12, 13, 92
      5                            T2  37, 54, 69
      6                             I  16, 18, 89
      7                            II  15, 17, 88
      8                           III  17, 19, 89
      9                            T3  32, 43, 74
      10                            I  16, 18, 89
      11                           II   9, 11, 82
      12                          III 14, 14, 100
      13                           T4  35, 50, 70
      14                            I  14, 15, 93
      15                           II  14, 17, 82
      16                          III 18, 18, 100

---

    Code
      tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2,
      id = id, statistic = ~ list(stage = "{n}"))
    Condition
      Error in `tbl_hierarchical()`:
      ! Values passed in the `statistic` argument must be strings with glue elements containing one or more of "n", "N", and "p".

# tbl_hierarchical(overall_row) works properly

    Code
      as.data.frame(tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = id, overall_row = TRUE))
    Output
           **Chemotherapy Treatment** **N = 200**
      1 Number of patients with event    50 (25%)
      2                        Drug A    45 (46%)
      3                        Drug B    44 (43%)

---

    Code
      as.data.frame(res)
    Output
           **Chemotherapy Treatment** **I**  \nN = 68 **II**  \nN = 68 **III**  \nN = 64
      1 Number of patients with event        40 (59%)         38 (56%)          39 (61%)
      2                        Drug A        25 (71%)         27 (84%)          23 (74%)
      3                        Drug B        24 (73%)         26 (72%)          26 (79%)

---

    Code
      tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = id, overall_row = "test")
    Condition
      Error in `tbl_hierarchical()`:
      ! The `overall_row` argument must be class <logical>, not a string.

# tbl_hierarchical(label) works properly

    Code
      as.data.frame(res)
    Output
         **My Stage**  \n    **My Grade** **N = 200**
      1                                T1    36 (68%)
      2                                 I    14 (82%)
      3                                II    20 (87%)
      4                               III    12 (92%)
      5                                T2    37 (69%)
      6                                 I    16 (89%)
      7                                II    15 (88%)
      8                               III    17 (89%)
      9                                T3    32 (74%)
      10                                I    16 (89%)
      11                               II     9 (82%)
      12                              III   14 (100%)
      13                               T4    35 (70%)
      14                                I    14 (93%)
      15                               II    14 (82%)
      16                              III   18 (100%)

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

# tbl_hierarchical(digits) works properly

    Code
      as.data.frame(res)
    Output
         **T Stage**  \n    **Grade**     **N = 200**
      1                            T1        36 (68%)
      2                             I  14,0 (82.353%)
      3                            II  20,0 (86.957%)
      4                           III  12,0 (92.308%)
      5                            T2        37 (69%)
      6                             I  16,0 (88.889%)
      7                            II  15,0 (88.235%)
      8                           III  17,0 (89.474%)
      9                            T3        32 (74%)
      10                            I  16,0 (88.889%)
      11                           II   9,0 (81.818%)
      12                          III 14,0 (100.000%)
      13                           T4        35 (70%)
      14                            I  14,0 (93.333%)
      15                           II  14,0 (82.353%)
      16                          III 18,0 (100.000%)

---

    Code
      tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2,
      id = id, digits = "0")
    Condition
      Error in `internal_tbl_hierarchical()`:
      ! The `digits` argument must be a named list, list of formulas, a single formula, or empty.
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
      2               2 (2.3%)                           1 (1.2%)
      3                 0 (0%)                           2 (2.4%)
      4               1 (1.2%)                             0 (0%)
      5                   <NA>                               <NA>
      6                9 (10%)                           2 (2.4%)
      7               1 (1.2%)                           2 (2.4%)
      8                 0 (0%)                             0 (0%)
      9                   <NA>                               <NA>
      10              8 (9.3%)                           15 (18%)
      11              1 (1.2%)                           13 (15%)
      12                0 (0%)                             0 (0%)
      13                  <NA>                               <NA>
      14              4 (4.7%)                           2 (2.4%)
      15              2 (2.3%)                           1 (1.2%)
      16                0 (0%)                             0 (0%)
      17                  <NA>                               <NA>
      18              5 (5.8%)                           11 (13%)
      19              4 (4.7%)                           4 (4.8%)
      20                0 (0%)                             0 (0%)
         **Xanomeline Low Dose**  \nN = 84
      1                               <NA>
      2                             0 (0%)
      3                             0 (0%)
      4                             0 (0%)
      5                               <NA>
      6                           5 (6.0%)
      7                             0 (0%)
      8                             0 (0%)
      9                               <NA>
      10                          14 (17%)
      11                          11 (13%)
      12                          2 (2.4%)
      13                              <NA>
      14                          1 (1.2%)
      15                            0 (0%)
      16                            0 (0%)
      17                              <NA>
      18                          6 (7.1%)
      19                           9 (11%)
      20                            0 (0%)

---

    Code
      as.data.frame(res)
    Output
              **Primary System Organ Class**  \n    **AESEV** **N = 254**
      1                                     CARDIAC DISORDERS    6 (2.4%)
      2                                                  MILD    3 (1.2%)
      3                                              MODERATE    2 (0.8%)
      4                                                SEVERE    1 (0.4%)
      5                            GASTROINTESTINAL DISORDERS   19 (7.5%)
      6                                                  MILD   16 (6.3%)
      7                                              MODERATE    3 (1.2%)
      8                                                SEVERE      0 (0%)
      9  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS    64 (25%)
      10                                                 MILD    37 (15%)
      11                                             MODERATE   25 (9.8%)
      12                                               SEVERE    2 (0.8%)
      13                          INFECTIONS AND INFESTATIONS   10 (3.9%)
      14                                                 MILD    7 (2.8%)
      15                                             MODERATE    3 (1.2%)
      16                                               SEVERE      0 (0%)
      17               SKIN AND SUBCUTANEOUS TISSUE DISORDERS    39 (15%)
      18                                                 MILD   22 (8.7%)
      19                                             MODERATE   17 (6.7%)
      20                                               SEVERE      0 (0%)

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

# tbl_hierarchical_count(digits) works properly

    Code
      as.data.frame(res)
    Output
         **T Stage**  \n    **Grade** Overall
      1                            T1    53,0
      2                             I    17,0
      3                            II    23,0
      4                           III    13,0
      5                            T2    54,0
      6                             I    18,0
      7                            II    17,0
      8                           III    19,0
      9                            T3    43,0
      10                            I    18,0
      11                           II    11,0
      12                          III    14,0
      13                           T4    50,0
      14                            I    15,0
      15                           II    17,0
      16                          III    18,0

---

    Code
      tbl_hierarchical_count(data = trial, variables = c(stage, grade), digits = n ~
      2)
    Condition
      Error in `tbl_hierarchical_count()`:
      ! Error processing `digits` argument.
      ! Can't select columns that don't exist. x Column `n` doesn't exist.
      i Select among columns "stage", "grade", and "..ard_hierarchical_overall.."

# tbl_hierarchical_count with 10+ hierarchy variables

    Code
      as.data.frame(res)
    Output
          **x1**  \n    **x2**  \n        **x3**  \n            **x4**  \n                **x5**  \n                    **x6**  \n                        **x7**  \n                            **x8**  \n                                **x9**  \n                          ...
      1                                                                                                                                                                                                                                                                         A
      2                                                                                                                                                                                                                                                                         C
      3                                                                                                                                                                                                                                                                         E
      4                                                                                                                                                                                                                                                                         H
      5                                                                                                                                                                                                                                                                         I
      6                                                                                                                                                                                                                                                                         K
      7                                                                                                                                                                                                                                                                         N
      8                                                                                                                                                                                                                                                                         O
      9                                                                                                                                                                                                                                                                         Q
      10                                                                                                                                                                                                                                                                        S
      11                                                                                                                                                                                                                                                                        J
      12                                                                                                                                                                                                                                                                        K
      13                                                                                                                                                                                                                                                                        M
      14                                                                                                                                                                                                                                                                        P
      15                                                                                                                                                                                                                                                                        Q
      16                                                                                                                                                                                                                                                                        S
      17                                                                                                                                                                                                                                                                        L
      18                                                                                                                                                                                                                                                                        M
      19                                                                                                                                                                                                                                                                        P
      20                                                                                                                                                                                                                                                                        R
      21                                                                                                                                                                                                                                                                        T
      22                                                                                                                                                                                                                                                                        F
      23                                                                                                                                                                                                                                                                        G
      24                                                                                                                                                                                                                                                                        I
      25                                                                                                                                                                                                                                                                        K
      26                                                                                                                                                                                                                                                                        N
      27                                                                                                                                                                                                                                                                        O
      28                                                                                                                                                                                                                                                                        Q
      29                                                                                                                                                                                                                                                                        S
      30                                                                                                                                                                                                                                                                        L
      31                                                                                                                                                                                                                                                                        M
      32                                                                                                                                                                                                                                                                        O
      33                                                                                                                                                                                                                                                                        R
      34                                                                                                                                                                                                                                                                        S
      35                                                                                                                                                                                                                                                                        P
      36                                                                                                                                                                                                                                                                        R
      37                                                                                                                                                                                                                                                                        T
      38                                                                                                                                                                                                                                                                        H
      39                                                                                                                                                                                                                                                                        I
      40                                                                                                                                                                                                                                                                        K
      41                                                                                                                                                                                                                                                                        M
      42                                                                                                                                                                                                                                                                        O
      43                                                                                                                                                                                                                                                                        Q
      44                                                                                                                                                                                                                                                                        S
      45                                                                                                                                                                                                                                                                        J
      46                                                                                                                                                                                                                                                                        L
      47                                                                                                                                                                                                                                                                        N
      48                                                                                                                                                                                                                                                                        P
      49                                                                                                                                                                                                                                                                        R
      50                                                                                                                                                                                                                                                                        T
      51                                                                                                                                                                                                                                                                        D
      52                                                                                                                                                                                                                                                                        E
      53                                                                                                                                                                                                                                                                        G
      54                                                                                                                                                                                                                                                                        I
      55                                                                                                                                                                                                                                                                        L
      56                                                                                                                                                                                                                                                                        M
      57                                                                                                                                                                                                                                                                        O
      58                                                                                                                                                                                                                                                                        Q
      59                                                                                                                                                                                                                                                                        T
      60                                                                                                                                                                                                                                                                        N
      61                                                                                                                                                                                                                                                                        P
      62                                                                                                                                                                                                                                                                        Q
      63                                                                                                                                                                                                                                                                        T
      64                                                                                                                                                                                                                                                                        J
      65                                                                                                                                                                                                                                                                        K
      66                                                                                                                                                                                                                                                                        N
      67                                                                                                                                                                                                                                                                        P
      68                                                                                                                                                                                                                                                                        Q
      69                                                                                                                                                                                                                                                                        T
      70                                                                                                                                                                                                                                                                        L
      71                                                                                                                                                                                                                                                                        N
      72                                                                                                                                                                                                                                                                        O
      73                                                                                                                                                                                                                                                                        R
      74                                                                                                                                                                                                                                                                        T
      75                                                                                                                                                                                                                                                                        H
      76                                                                                                                                                                                                                                                                        I
      77                                                                                                                                                                                                                                                                        L
      78                                                                                                                                                                                                                                                                        N
      79                                                                                                                                                                                                                                                                        P
      80                                                                                                                                                                                                                                                                        R
      81                                                                                                                                                                                                                                                                        T
      82                                                                                                                                                                                                                                                                        J
      83                                                                                                                                                                                                                                                                        K
      84                                                                                                                                                                                                                                                                        N
      85                                                                                                                                                                                                                                                                        O
      86                                                                                                                                                                                                                                                                        Q
      87                                                                                                                                                                                                                                                                        T
      88                                                                                                                                                                                                                                                                        F
      89                                                                                                                                                                                                                                                                        G
      90                                                                                                                                                                                                                                                                        I
      91                                                                                                                                                                                                                                                                        L
      92                                                                                                                                                                                                                                                                        N
      93                                                                                                                                                                                                                                                                        O
      94                                                                                                                                                                                                                                                                        Q
      95                                                                                                                                                                                                                                                                        T
      96                                                                                                                                                                                                                                                                        J
      97                                                                                                                                                                                                                                                                        K
      98                                                                                                                                                                                                                                                                        N
      99                                                                                                                                                                                                                                                                        O
      100                                                                                                                                                                                                                                                                       R
      101                                                                                                                                                                                                                                                                       S
      102                                                                                                                                                                                                                                                                       L
      103                                                                                                                                                                                                                                                                       M
      104                                                                                                                                                                                                                                                                       P
      105                                                                                                                                                                                                                                                                       Q
      106                                                                                                                                                                                                                                                                       S
      107                                                                                                                                                                                                                                                                       H
      108                                                                                                                                                                                                                                                                       I
      109                                                                                                                                                                                                                                                                       K
      110                                                                                                                                                                                                                                                                       M
      111                                                                                                                                                                                                                                                                       P
      112                                                                                                                                                                                                                                                                       Q
      113                                                                                                                                                                                                                                                                       T
      114                                                                                                                                                                                                                                                                       R
      115                                                                                                                                                                                                                                                                       T
      116                                                                                                                                                                                                                                                                       N
      117                                                                                                                                                                                                                                                                       P
      118                                                                                                                                                                                                                                                                       R
      119                                                                                                                                                                                                                                                                       T
      120                                                                                                                                                                                                                                                                       B
      121                                                                                                                                                                                                                                                                       C
      122                                                                                                                                                                                                                                                                       E
      123                                                                                                                                                                                                                                                                       G
      124                                                                                                                                                                                                                                                                       J
      125                                                                                                                                                                                                                                                                       K
      126                                                                                                                                                                                                                                                                       M
      127                                                                                                                                                                                                                                                                       O
      128                                                                                                                                                                                                                                                                       Q
      129                                                                                                                                                                                                                                                                       T
      130                                                                                                                                                                                                                                                                       F
      131                                                                                                                                                                                                                                                                       G
      132                                                                                                                                                                                                                                                                       J
      133                                                                                                                                                                                                                                                                       K
      134                                                                                                                                                                                                                                                                       M
      135                                                                                                                                                                                                                                                                       O
      136                                                                                                                                                                                                                                                                       Q
      137                                                                                                                                                                                                                                                                       S
      138                                                                                                                                                                                                                                                                       H
      139                                                                                                                                                                                                                                                                       I
      140                                                                                                                                                                                                                                                                       K
      141                                                                                                                                                                                                                                                                       M
      142                                                                                                                                                                                                                                                                       P
      143                                                                                                                                                                                                                                                                       R
      144                                                                                                                                                                                                                                                                       T
      145                                                                                                                                                                                                                                                                       D
      146                                                                                                                                                                                                                                                                       E
      147                                                                                                                                                                                                                                                                       H
      148                                                                                                                                                                                                                                                                       J
      149                                                                                                                                                                                                                                                                       K
      150                                                                                                                                                                                                                                                                       N
      151                                                                                                                                                                                                                                                                       O
      152                                                                                                                                                                                                                                                                       Q
      153                                                                                                                                                                                                                                                                       T
      154                                                                                                                                                                                                                                                                       L
      155                                                                                                                                                                                                                                                                       M
      156                                                                                                                                                                                                                                                                       P
      157                                                                                                                                                                                                                                                                       Q
      158                                                                                                                                                                                                                                                                       T
      159                                                                                                                                                                                                                                                                       F
      160                                                                                                                                                                                                                                                                       G
      161                                                                                                                                                                                                                                                                       I
      162                                                                                                                                                                                                                                                                       K
      163                                                                                                                                                                                                                                                                       N
      164                                                                                                                                                                                                                                                                       O
      165                                                                                                                                                                                                                                                                       Q
      166                                                                                                                                                                                                                                                                       T
      167                                                                                                                                                                                                                                                                       L
      168                                                                                                                                                                                                                                                                       M
      169                                                                                                                                                                                                                                                                       O
      170                                                                                                                                                                                                                                                                       R
      171                                                                                                                                                                                                                                                                       T
      172                                                                                                                                                                                                                                                                       J
      173                                                                                                                                                                                                                                                                       K
      174                                                                                                                                                                                                                                                                       M
      175                                                                                                                                                                                                                                                                       O
      176                                                                                                                                                                                                                                                                       R
      177                                                                                                                                                                                                                                                                       T
      178                                                                                                                                                                                                                                                                       L
      179                                                                                                                                                                                                                                                                       M
      180                                                                                                                                                                                                                                                                       P
      181                                                                                                                                                                                                                                                                       R
      182                                                                                                                                                                                                                                                                       T
      183                                                                                                                                                                                                                                                                       H
      184                                                                                                                                                                                                                                                                       J
      185                                                                                                                                                                                                                                                                       L
      186                                                                                                                                                                                                                                                                       N
      187                                                                                                                                                                                                                                                                       P
      188                                                                                                                                                                                                                                                                       Q
      189                                                                                                                                                                                                                                                                       T
          Overall
      1      <NA>
      2      <NA>
      3      <NA>
      4      <NA>
      5      <NA>
      6      <NA>
      7      <NA>
      8      <NA>
      9      <NA>
      10        1
      11     <NA>
      12     <NA>
      13     <NA>
      14     <NA>
      15     <NA>
      16        1
      17     <NA>
      18     <NA>
      19     <NA>
      20     <NA>
      21        1
      22     <NA>
      23     <NA>
      24     <NA>
      25     <NA>
      26     <NA>
      27     <NA>
      28     <NA>
      29        1
      30     <NA>
      31     <NA>
      32     <NA>
      33     <NA>
      34        1
      35     <NA>
      36     <NA>
      37        1
      38     <NA>
      39     <NA>
      40     <NA>
      41     <NA>
      42     <NA>
      43     <NA>
      44        1
      45     <NA>
      46     <NA>
      47     <NA>
      48     <NA>
      49     <NA>
      50        1
      51     <NA>
      52     <NA>
      53     <NA>
      54     <NA>
      55     <NA>
      56     <NA>
      57     <NA>
      58     <NA>
      59        1
      60     <NA>
      61     <NA>
      62     <NA>
      63        1
      64     <NA>
      65     <NA>
      66     <NA>
      67     <NA>
      68     <NA>
      69        1
      70     <NA>
      71     <NA>
      72     <NA>
      73     <NA>
      74        1
      75     <NA>
      76     <NA>
      77     <NA>
      78     <NA>
      79     <NA>
      80     <NA>
      81        1
      82     <NA>
      83     <NA>
      84     <NA>
      85     <NA>
      86     <NA>
      87        1
      88     <NA>
      89     <NA>
      90     <NA>
      91     <NA>
      92     <NA>
      93     <NA>
      94     <NA>
      95        1
      96     <NA>
      97     <NA>
      98     <NA>
      99     <NA>
      100    <NA>
      101       1
      102    <NA>
      103    <NA>
      104    <NA>
      105    <NA>
      106       1
      107    <NA>
      108    <NA>
      109    <NA>
      110    <NA>
      111    <NA>
      112    <NA>
      113       1
      114    <NA>
      115       1
      116    <NA>
      117    <NA>
      118    <NA>
      119       1
      120    <NA>
      121    <NA>
      122    <NA>
      123    <NA>
      124    <NA>
      125    <NA>
      126    <NA>
      127    <NA>
      128    <NA>
      129       1
      130    <NA>
      131    <NA>
      132    <NA>
      133    <NA>
      134    <NA>
      135    <NA>
      136    <NA>
      137       1
      138    <NA>
      139    <NA>
      140    <NA>
      141    <NA>
      142    <NA>
      143    <NA>
      144       1
      145    <NA>
      146    <NA>
      147    <NA>
      148    <NA>
      149    <NA>
      150    <NA>
      151    <NA>
      152    <NA>
      153       1
      154    <NA>
      155    <NA>
      156    <NA>
      157    <NA>
      158       1
      159    <NA>
      160    <NA>
      161    <NA>
      162    <NA>
      163    <NA>
      164    <NA>
      165    <NA>
      166       1
      167    <NA>
      168    <NA>
      169    <NA>
      170    <NA>
      171       1
      172    <NA>
      173    <NA>
      174    <NA>
      175    <NA>
      176    <NA>
      177       1
      178    <NA>
      179    <NA>
      180    <NA>
      181    <NA>
      182       1
      183    <NA>
      184    <NA>
      185    <NA>
      186    <NA>
      187    <NA>
      188    <NA>
      189       1

# tbl_hierarchical_count table_body enables sorting

    Code
      res$table_body
    Output
      # A tibble: 21 x 11
         row_type group1                       group1_level group2 group2_level                                         var_label variable                     label                                                stat_1   stat_2   stat_3  
         <chr>    <chr>                        <chr>        <chr>  <chr>                                                <chr>     <chr>                        <chr>                                                <chr>    <chr>    <chr>   
       1 level    ..ard_hierarchical_overall.. <NA>         <NA>   <NA>                                                 <NA>      ..ard_hierarchical_overall.. Number of patients with event                        26 (30%) 42 (50%) 40 (48%)
       2 level    SEX                          F            <NA>   <NA>                                                 <NA>      SEX                          F                                                    13 (25%) 18 (45%) 23 (46%)
       3 level    SEX                          F            AESOC  CARDIAC DISORDERS                                    <NA>      AESOC                        CARDIAC DISORDERS                                    0 (0%)   1 (2.5%) 0 (0%)  
       4 level    SEX                          F            AESOC  CARDIAC DISORDERS                                    <NA>      AETERM                       ATRIOVENTRICULAR BLOCK SECOND DEGREE                 0 (0%)   1 (2.5%) 0 (0%)  
       5 level    SEX                          F            AESOC  GASTROINTESTINAL DISORDERS                           <NA>      AESOC                        GASTROINTESTINAL DISORDERS                           3 (5.7%) 0 (0%)   3 (6.0%)
       6 level    SEX                          F            AESOC  GASTROINTESTINAL DISORDERS                           <NA>      AETERM                       DIARRHOEA                                            3 (5.7%) 0 (0%)   3 (6.0%)
       7 level    SEX                          F            AESOC  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS <NA>      AESOC                        GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS 5 (9.4%) 11 (28%) 13 (26%)
       8 level    SEX                          F            AESOC  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS <NA>      AETERM                       APPLICATION SITE ERYTHEMA                            2 (3.8%) 5 (13%)  5 (10%) 
       9 level    SEX                          F            AESOC  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS <NA>      AETERM                       APPLICATION SITE PRURITUS                            4 (7.5%) 10 (25%) 12 (24%)
      10 level    SEX                          F            AESOC  SKIN AND SUBCUTANEOUS TISSUE DISORDERS               <NA>      AESOC                        SKIN AND SUBCUTANEOUS TISSUE DISORDERS               6 (11%)  7 (18%)  9 (18%) 
      # i 11 more rows

