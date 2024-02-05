# standard tbl_summary() creates correct

    Code
      as.data.frame(tbl_summary(data = trial))
    Output
             **Characteristic**       **N = 200**
      1  Chemotherapy Treatment              <NA>
      2                  Drug A          98 (49%)
      3                  Drug B         102 (51%)
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.41)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1          53 (27%)
      10                     T2          54 (27%)
      11                     T3          43 (22%)
      12                     T4          50 (25%)
      13                  Grade              <NA>
      14                      I          68 (34%)
      15                     II          68 (34%)
      16                    III          64 (32%)
      17         Tumor Response          61 (32%)
      18                Unknown                 7
      19           Patient Died         112 (56%)
      20 Months to Death/Censor 22.4 (15.9, 24.0)

---

    Code
      as.data.frame(tbl_summary(data = mtcars))
    Output
         **Characteristic**           **N = 32**
      1                 mpg    19.2 (15.4, 22.8)
      2                 cyl                 <NA>
      3                   4             11 (34%)
      4                   6              7 (22%)
      5                   8             14 (44%)
      6                disp       196 (121, 334)
      7                  hp        123 (96, 180)
      8                drat    3.70 (3.08, 3.92)
      9                  wt    3.33 (2.54, 3.65)
      10               qsec 17.71 (16.89, 18.90)
      11                 vs             14 (44%)
      12                 am             13 (41%)
      13               gear                 <NA>
      14                  3             15 (47%)
      15                  4             12 (38%)
      16                  5              5 (16%)
      17               carb                 <NA>
      18                  1              7 (22%)
      19                  2             10 (31%)
      20                  3             3 (9.4%)
      21                  4             10 (31%)
      22                  6             1 (3.1%)
      23                  8             1 (3.1%)

---

    Code
      as.data.frame(tbl_summary(data = iris))
    Output
        **Characteristic**       **N = 150**
      1       Sepal.Length 5.80 (5.10, 6.40)
      2        Sepal.Width 3.00 (2.80, 3.30)
      3       Petal.Length 4.35 (1.60, 5.10)
      4        Petal.Width 1.30 (0.30, 1.80)
      5            Species              <NA>
      6             setosa          50 (33%)
      7         versicolor          50 (33%)
      8          virginica          50 (33%)

# tbl_summary(by) creates output without error/warning

    Code
      as.data.frame(tbl_summary(data = trial, by = trt))
    Output
             **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1                     Age          46 (37, 60)           48 (39, 56)
      2                 Unknown                    7                     4
      3    Marker Level (ng/mL)    0.84 (0.23, 1.60)     0.52 (0.18, 1.21)
      4                 Unknown                    6                     4
      5                 T Stage                 <NA>                  <NA>
      6                      T1             28 (29%)              25 (25%)
      7                      T2             25 (26%)              29 (28%)
      8                      T3             22 (22%)              21 (21%)
      9                      T4             23 (23%)              27 (26%)
      10                  Grade                 <NA>                  <NA>
      11                      I             35 (36%)              33 (32%)
      12                     II             32 (33%)              36 (35%)
      13                    III             31 (32%)              33 (32%)
      14         Tumor Response             28 (29%)              33 (34%)
      15                Unknown                    3                     4
      16           Patient Died             52 (53%)              60 (59%)
      17 Months to Death/Censor    23.5 (17.4, 24.0)     21.2 (14.5, 24.0)

---

    Code
      as.data.frame(tbl_summary(data = mtcars, by = am))
    Output
         **Characteristic**      **0**  \nN = 19      **1**  \nN = 13
      1                 mpg    17.3 (14.7, 19.2)    22.8 (21.0, 30.4)
      2                 cyl                 <NA>                 <NA>
      3                   4              3 (16%)              8 (62%)
      4                   6              4 (21%)              3 (23%)
      5                   8             12 (63%)              2 (15%)
      6                disp       276 (168, 360)        120 (79, 160)
      7                  hp       175 (110, 205)        109 (66, 113)
      8                drat    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)
      9                  wt    3.52 (3.44, 3.85)    2.32 (1.94, 2.78)
      10               qsec 17.82 (17.05, 19.44) 17.02 (16.46, 18.61)
      11                 vs              7 (37%)              7 (54%)
      12               gear                 <NA>                 <NA>
      13                  3             15 (79%)               0 (0%)
      14                  4              4 (21%)              8 (62%)
      15                  5               0 (0%)              5 (38%)
      16               carb                 <NA>                 <NA>
      17                  1              3 (16%)              4 (31%)
      18                  2              6 (32%)              4 (31%)
      19                  3              3 (16%)               0 (0%)
      20                  4              7 (37%)              3 (23%)
      21                  6               0 (0%)             1 (7.7%)
      22                  8               0 (0%)             1 (7.7%)

---

    Code
      as.data.frame(tbl_summary(data = iris, by = Species))
    Output
        **Characteristic** **setosa**  \nN = 50 **versicolor**  \nN = 50
      1       Sepal.Length    5.00 (4.80, 5.20)        5.90 (5.60, 6.30)
      2        Sepal.Width    3.40 (3.20, 3.70)        2.80 (2.50, 3.00)
      3       Petal.Length    1.50 (1.40, 1.60)        4.35 (4.00, 4.60)
      4        Petal.Width    0.20 (0.20, 0.30)        1.30 (1.20, 1.50)
        **virginica**  \nN = 50
      1       6.50 (6.20, 6.90)
      2       3.00 (2.80, 3.20)
      3       5.55 (5.10, 5.90)
      4       2.00 (1.80, 2.30)

# tbl_summary(label) allows for named list input

    Code
      as.data.frame(tbl_summary(mtcars, by = am, label = list(mpg = "New mpg", cyl = "New cyl"),
      include = c(mpg, cyl)))
    Output
        **Characteristic**   **0**  \nN = 19   **1**  \nN = 13
      1            New mpg 17.3 (14.7, 19.2) 22.8 (21.0, 30.4)
      2            New cyl              <NA>              <NA>
      3                  4           3 (16%)           8 (62%)
      4                  6           4 (21%)           3 (23%)
      5                  8          12 (63%)           2 (15%)

# tbl_summary(sort) works

    Code
      tbl_summary(mtcars, sort = list(all_categorical() ~ c("frequency", "two")))
    Condition
      Error in `tbl_summary()`:
      ! Error in argument `sort` for column "cyl": value must be one of "alphanumeric" and "frequency".

---

    Code
      tbl_summary(mtcars, sort = list(all_categorical() ~ "freq5555uency"))
    Condition
      Error in `tbl_summary()`:
      ! Error in argument `sort` for column "cyl": value must be one of "alphanumeric" and "frequency".

# tbl_summary(value) works

    Code
      as.data.frame(tbl_summary(trial, value = "grade" ~ "III", include = grade))
    Output
        **Characteristic** **N = 200**
      1              Grade    64 (32%)

