# tbl_summary(data)

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

# tbl_summary(data) errors properly

    Code
      tbl_summary()
    Condition
      Error in `tbl_summary()`:
      ! The `data` argument cannot be missing.

---

    Code
      tbl_summary(data = letters)
    Condition
      Error in `tbl_summary()`:
      ! The `data` argument must be class <data.frame>, not a character vector.

---

    Code
      tbl_summary(data = dplyr::tibble())
    Condition
      Error in `tbl_summary()`:
      ! Expecting `data` argument to have at least 1 row and 1 column.

# tbl_summary(by)

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

# tbl_summary(by) errors properly

    Code
      tbl_summary(mtcars, by = c("mpg", "am"))
    Condition
      Error in `tbl_summary()`:
      ! The `by` argument must be length 1 or empty.
      i Use `tbl_strata()` for more than one `by` variable.

# tbl_summary(label)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic**   **0**  \nN = 19   **1**  \nN = 13
      1            New mpg 17.3 (14.7, 19.2) 22.8 (21.0, 30.4)
      2            New cyl              <NA>              <NA>
      3                  4           3 (16%)           8 (62%)
      4                  6           4 (21%)           3 (23%)
      5                  8          12 (63%)           2 (15%)

# tbl_summary(label) errors properly

    Code
      tbl_summary(trial["age"], label = list(age = letters))
    Condition
      Error in `tbl_summary()`:
      ! Error in argument `label` for column "age": value must be a string.

---

    Code
      tbl_summary(trial["age"], label = letters)
    Condition
      Error in `tbl_summary()`:
      ! The `label` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

# tbl_summary(statistic) errors properly

    Code
      tbl_summary(trial, include = response, statistic = ~"{n} ({not_a_statistic})")
    Condition
      Error in `tbl_summary()`:
      ! Statistic "not_a_statistic" is not available for variable "response".
      i Select among "n", "N", "p", "N_obs", "N_miss", "N_nonmiss", "p_miss", and "p_nonmiss".

---

    Code
      tbl_summary(trial, include = age, statistic = ~"({not_a_summary_statistic})")
    Condition
      Error in `tbl_summary()`:
      ! Problem with the `statistic` argument.
      Error converting string "not_a_summary_statistic" to a function.
      i Is the name spelled correctly and available?

# tbl_summary(statistic,type) errors

    Code
      tbl_summary(trial, include = age, statistic = ~ c("{mean}", "{sd}"))
    Condition
      Error in `tbl_summary()`:
      ! The `statistic` argument value for variable "age" must be a string, but is a character vector.
      i Did you mean to set `type = list(age = "continuous2")` for a multi-line summary?

---

    Code
      tbl_summary(trial, include = grade, statistic = ~ c("{mean}", "{sd}"))
    Condition
      Error in `tbl_summary()`:
      ! The `statistic` argument value for variable "grade" must be a string, but is a character vector.

# tbl_summary(type)

    Code
      dplyr::select(getElement(tbl_summary(trial, include = c(age, marker, response,
        stage), type = list(age = "continuous", marker = "continuous2", response = "dichotomous",
        state = "categorical"), missing = "no"), "table_body"), variable, var_type,
      row_type, label)
    Output
      # A tibble: 9 x 4
        variable var_type    row_type label               
        <chr>    <chr>       <chr>    <chr>               
      1 age      continuous  label    Age                 
      2 marker   continuous2 label    Marker Level (ng/mL)
      3 marker   continuous2 level    Median (Q1, Q3)     
      4 response dichotomous label    Tumor Response      
      5 stage    categorical label    T Stage             
      6 stage    categorical level    T1                  
      7 stage    categorical level    T2                  
      8 stage    categorical level    T3                  
      9 stage    categorical level    T4                  

# tbl_summary(type) proper errors/messages

    Code
      tbl <- tbl_summary(trial, include = grade, type = grade ~ "continuous")
    Message
      The following errors were returned during `tbl_summary()`:
      x For variable `grade` and "median" statistic: need numeric data
      x For variable `grade` and "p25" and "p75" statistics: (unordered) factors are not allowed

---

    Code
      tbl_summary(trial, include = grade, type = grade ~ "dichotomous", value = grade ~
        "IV")
    Condition
      Error in `tbl_summary()`:
      ! Error in argument `value` for variable "grade".
      i A value of "IV" was passed, but must be one of I, II, and III.
      i To summarize this value, use `forcats::fct_expand()` to add "IV" as a level.

---

    Code
      tbl_summary(trial, include = grade, type = grade ~ "dichotomous")
    Condition
      Error in `tbl_summary()`:
      ! Error in argument `value` for variable "grade".
      i Summary type is "dichotomous" but no summary value has been assigned.

# tbl_summary(value)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **N = 200**
      1              Grade    64 (32%)
      2     Tumor Response    61 (32%)
      3            Unknown           7

# tbl_summary(value) errors properly

    Code
      tbl_summary(trial, value = "grade" ~ "IV", include = c(grade, response))
    Condition
      Error in `tbl_summary()`:
      ! Error in argument `value` for variable "grade".
      i A value of "IV" was passed, but must be one of I, II, and III.
      i To summarize this value, use `forcats::fct_expand()` to add "IV" as a level.

# tbl_summary(missing)

    Code
      tbl_summary(trial, missing = "NOT AN OPTION")
    Condition
      Error in `tbl_summary()`:
      ! `missing` must be one of "ifany", "no", or "always", not "NOT AN OPTION".

# tbl_summary(missing_text)

    Code
      as.data.frame(tbl_summary(trial, include = response, missing_text = "(MISSING)"),
      col_label = FALSE)
    Output
                 label   stat_0
      1 Tumor Response 61 (32%)
      2      (MISSING)        7

---

    Code
      tbl_summary(trial, include = response, missing_text = letters)
    Condition
      Error in `tbl_summary()`:
      ! The `missing_text` argument must be a string, not a character vector.

---

    Code
      tbl_summary(trial, include = response, missing_text = 10L)
    Condition
      Error in `tbl_summary()`:
      ! The `missing_text` argument must be a string, not an integer.

# tbl_summary(missing_stat)

    Code
      tbl_summary(trial, include = response, missing_stat = letters)
    Condition
      Error in `tbl_summary()`:
      ! The `missing_stat` argument must be a string, not a character vector.

---

    Code
      tbl_summary(trial, include = response, missing_stat = 10L)
    Condition
      Error in `tbl_summary()`:
      ! The `missing_stat` argument must be a string, not an integer.

# tbl_summary(sort) errors properly

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

# tbl_summary(percent)

    Code
      as.data.frame(tbl_summary(trial, by = trt, include = grade, percent = "column",
        statistic = ~"{p}%"), col_labels = FALSE)
    Output
        label stat_1 stat_2
      1 Grade   <NA>   <NA>
      2     I    36%    32%
      3    II    33%    35%
      4   III    32%    32%

---

    Code
      as.data.frame(tbl_summary(trial, by = trt, include = grade, percent = "row",
        statistic = ~"{p}%"), col_labels = FALSE)
    Output
        label stat_1 stat_2
      1 Grade   <NA>   <NA>
      2     I    51%    49%
      3    II    47%    53%
      4   III    48%    52%

---

    Code
      as.data.frame(tbl_summary(trial, by = trt, include = grade, percent = "cell",
        statistic = ~"{p}%"), col_labels = FALSE)
    Output
        label stat_1 stat_2
      1 Grade   <NA>   <NA>
      2     I    18%    17%
      3    II    16%    18%
      4   III    16%    17%

---

    Code
      tbl_summary(trial, by = trt, include = grade, percent = letters, statistic = ~
        "{p}%")
    Condition
      Error in `tbl_summary()`:
      ! `percent` must be one of "column", "row", or "cell", not "a".

# tbl_summary() edge case of warning condition printing

    Code
      as_kable(tbl_summary(dplyr::tibble(by_var = as.factor(c(rep("cohort_1", 3), rep(
        "cohort_2", 3))), continuous_var = c(NA, NA, NA, 1, 2, 3)), by = by_var,
      type = continuous_var ~ "continuous", statistic = continuous_var ~
        "{min}, {max}"), format = "pipe")
    Message
      The following warnings were returned during `as_kable()`:
      ! For variable `continuous_var` (`by_var = "cohort_1"`) and "min" statistic: no non-missing arguments to min; returning Inf
      ! For variable `continuous_var` (`by_var = "cohort_1"`) and "max" statistic: no non-missing arguments to max; returning -Inf
    Output
      
      
      |**Characteristic** | **cohort_1**  N = 3 | **cohort_2**  N = 3 |
      |:------------------|:-------------------:|:-------------------:|
      |continuous_var     |      Inf, -Inf      |     1.00, 3.00      |
      |Unknown            |          3          |          0          |

