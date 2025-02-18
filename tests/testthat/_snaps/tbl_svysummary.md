# tbl_svysummary(data)

    Code
      as.data.frame(tbl_svysummary(data = svy_titanic))
    Output
         **Characteristic**  **N = 2,201**
      1               Class           <NA>
      2                 1st      325 (15%)
      3                 2nd      285 (13%)
      4                 3rd      706 (32%)
      5                Crew      885 (40%)
      6                 Sex           <NA>
      7                Male    1,731 (79%)
      8              Female      470 (21%)
      9                 Age           <NA>
      10              Child     109 (5.0%)
      11              Adult    2,092 (95%)
      12           Survived      711 (32%)
      13               Freq 192 (118, 670)

---

    Code
      as.data.frame(tbl_svysummary(data = svy_mtcars))
    Output
         **Characteristic**           **N = 32**
      1                 mpg          19 (15, 23)
      2                 cyl                 <NA>
      3                   4             11 (34%)
      4                   6              7 (22%)
      5                   8             14 (44%)
      6                disp       168 (120, 318)
      7                  hp        123 (95, 180)
      8                drat    3.69 (3.08, 3.92)
      9                  wt    3.22 (2.47, 3.57)
      10               qsec 17.60 (16.87, 18.90)
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

# tbl_svysummary(data) errors properly

    Code
      tbl_svysummary()
    Condition
      Error in `tbl_svysummary()`:
      ! The `data` argument cannot be missing.

---

    Code
      tbl_svysummary(data = letters)
    Condition
      Error in `tbl_svysummary()`:
      ! The `data` argument must be class <survey.design>, not a character vector.

---

    Code
      tbl_svysummary(data = dplyr::tibble())
    Condition
      Error in `tbl_svysummary()`:
      ! The `data` argument must be class <survey.design>, not a tibble.

# tbl_svysummary(by)

    Code
      as.data.frame(tbl_svysummary(data = svy_mtcars, by = am))
    Output
         **Characteristic**      **0**  \nN = 19      **1**  \nN = 13
      1                 mpg          17 (15, 19)          23 (21, 30)
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
      as.data.frame(tbl_svysummary(data = svy_titanic, by = Survived))
    Output
         **Characteristic** **No**  \nN = 1,490 **Yes**  \nN = 711
      1               Class                <NA>               <NA>
      2                 1st          122 (8.2%)          203 (29%)
      3                 2nd           167 (11%)          118 (17%)
      4                 3rd           528 (35%)          178 (25%)
      5                Crew           673 (45%)          212 (30%)
      6                 Sex                <NA>               <NA>
      7                Male         1,364 (92%)          367 (52%)
      8              Female          126 (8.5%)          344 (48%)
      9                 Age                <NA>               <NA>
      10              Child           52 (3.5%)          57 (8.0%)
      11              Adult         1,438 (97%)          654 (92%)
      12               Freq      387 (154, 670)       80 (75, 192)

# tbl_svysummary(by) errors properly

    Code
      tbl_svysummary(svy_mtcars, by = c("mpg", "am"))
    Condition
      Error in `tbl_svysummary()`:
      ! The `by` argument must be length 1 or empty.
      i Use `tbl_strata()` for more than one `by` variable.

# tbl_svysummary(label)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **0**  \nN = 19 **1**  \nN = 13
      1            New mpg     17 (15, 19)     23 (21, 30)
      2            New cyl            <NA>            <NA>
      3                  4         3 (16%)         8 (62%)
      4                  6         4 (21%)         3 (23%)
      5                  8        12 (63%)         2 (15%)

# tbl_svysummary(label) errors properly

    Code
      tbl_svysummary(svy_trial, include = age, label = list(age = letters))
    Condition
      Error in `tbl_svysummary()`:
      ! Error in argument `label` for column "age": value must be a string.

---

    Code
      tbl_svysummary(svy_trial, include = age, label = letters)
    Condition
      Error in `tbl_svysummary()`:
      ! The `label` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

# tbl_svysummary(statistic) errors properly

    Code
      tbl_svysummary(svy_trial, include = response, statistic = ~
        "{n} ({not_a_statistic})")
    Condition
      Error in `tbl_svysummary()`:
      ! Statistic "not_a_statistic" is not available for variable "response".
      i Select among "N_nonmiss", "N_obs", "p_nonmiss", "N_miss", "p_miss", "N_miss_unweighted", "N_obs_unweighted", "p_miss_unweighted", "N_nonmiss_unweighted", "p_nonmiss_unweighted", "n", "N", "p", "p.std.error", "deff", "n_unweighted", "N_unweighted", and "p_unweighted".

---

    Code
      tbl_svysummary(svy_trial, include = age, statistic = ~
        "({not_a_summary_statistic})")
    Condition
      Error in `tbl_svysummary()`:
      ! Error in the values of the `statistic` argument for variable "age".
      i Values must be in "mean", "median", "min", "max", "sum", "var", "sd", "mean.std.error", "deff", and "p##"

# tbl_svysummary(statistic,type) errors

    Code
      tbl_svysummary(svy_trial, include = age, statistic = ~ c("{mean}", "{sd}"))
    Condition
      Error in `tbl_svysummary()`:
      ! The `statistic` argument value for variable "age" must be a string, but is a character vector.
      i Did you mean to set `type = list(age = "continuous2")` for a multi-line summary?

---

    Code
      tbl_svysummary(svy_trial, include = grade, statistic = ~ c("{mean}", "{sd}"))
    Condition
      Error in `tbl_svysummary()`:
      ! The `statistic` argument value for variable "grade" must be a string, but is a character vector.

# tbl_svysummary(type)

    Code
      dplyr::select(getElement(tbl_svysummary(svy_trial, include = c(age, marker,
        response, stage), type = list(age = "continuous", marker = "continuous2",
        response = "dichotomous", state = "categorical"), missing = "no"),
      "table_body"), variable, var_type, row_type, label)
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

# tbl_svysummary(type) proper errors/messages

    Code
      tbl <- tbl_svysummary(svy_trial, include = grade, type = grade ~ "continuous",
      statistic = ~"{min}")
    Message
      The following errors were returned during `tbl_svysummary()`:
      x For variable `grade` and "min" statistic: 'min' not meaningful for factors

---

    Code
      tbl_svysummary(svy_trial, include = grade, type = grade ~ "dichotomous", value = grade ~
        "IV")
    Condition
      Error in `tbl_svysummary()`:
      ! Error in argument `value` for variable "grade".
      i A value of "IV" was passed, but must be one of I, II, and III.
      i To summarize this value, use `forcats::fct_expand()` to add "IV" as a level.

---

    Code
      tbl_svysummary(svy_trial, include = grade, type = grade ~ "dichotomous")
    Condition
      Error in `tbl_svysummary()`:
      ! Error in argument `value` for variable "grade".
      i Summary type is "dichotomous" but no summary value has been assigned.

# tbl_svysummary(value)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **N = 200**
      1              Grade    64 (32%)
      2     Tumor Response    61 (32%)
      3            Unknown           7

# tbl_svysummary(value) errors properly

    Code
      tbl_svysummary(svy_trial, value = "grade" ~ "IV", include = c(grade, response))
    Condition
      Error in `tbl_svysummary()`:
      ! Error in argument `value` for variable "grade".
      i A value of "IV" was passed, but must be one of I, II, and III.
      i To summarize this value, use `forcats::fct_expand()` to add "IV" as a level.

# tbl_svysummary(missing)

    Code
      tbl_svysummary(svy_trial, missing = "NOT AN OPTION")
    Condition
      Error in `tbl_svysummary()`:
      ! `missing` must be one of "ifany", "no", or "always", not "NOT AN OPTION".

# tbl_svysummary(missing_text)

    Code
      as.data.frame(tbl_svysummary(svy_trial, include = response, missing_text = "(MISSING)"),
      col_label = FALSE)
    Output
                 label   stat_0
      1 Tumor Response 61 (32%)
      2      (MISSING)        7

---

    Code
      tbl_svysummary(svy_trial, include = response, missing_text = letters)
    Condition
      Error in `tbl_svysummary()`:
      ! The `missing_text` argument must be a string, not a character vector.

---

    Code
      tbl_svysummary(svy_trial, include = response, missing_text = 10L)
    Condition
      Error in `tbl_svysummary()`:
      ! The `missing_text` argument must be a string, not an integer.

# tbl_svysummary(missing_stat)

    Code
      tbl_svysummary(svy_trial, include = response, missing_stat = letters)
    Condition
      Error in `tbl_svysummary()`:
      ! The `missing_stat` argument must be a string, not a character vector.

---

    Code
      tbl_svysummary(svy_trial, include = response, missing_stat = 10L)
    Condition
      Error in `tbl_svysummary()`:
      ! The `missing_stat` argument must be a string, not an integer.

# tbl_svysummary(sort) errors properly

    Code
      tbl_svysummary(svy_mtcars, sort = list(all_categorical() ~ c("frequency", "two")))
    Condition
      Error in `tbl_svysummary()`:
      ! Error in argument `sort` for column "cyl": value must be one of "alphanumeric" and "frequency".

---

    Code
      tbl_svysummary(svy_mtcars, sort = list(all_categorical() ~ "freq5555uency"))
    Condition
      Error in `tbl_svysummary()`:
      ! Error in argument `sort` for column "cyl": value must be one of "alphanumeric" and "frequency".

# tbl_svysummary(percent)

    Code
      as.data.frame(tbl_svysummary(svy_trial, by = trt, include = grade, percent = "column",
        statistic = ~"{p}%"), col_labels = FALSE)
    Output
        label stat_1 stat_2
      1 Grade   <NA>   <NA>
      2     I    36%    32%
      3    II    33%    35%
      4   III    32%    32%

---

    Code
      as.data.frame(tbl_svysummary(svy_trial, by = trt, include = grade, percent = "row",
        statistic = ~"{p}%"), col_labels = FALSE)
    Output
        label stat_1 stat_2
      1 Grade   <NA>   <NA>
      2     I    51%    49%
      3    II    47%    53%
      4   III    48%    52%

---

    Code
      as.data.frame(tbl_svysummary(svy_trial, by = trt, include = grade, percent = "cell",
        statistic = ~"{p}%"), col_labels = FALSE)
    Output
        label stat_1 stat_2
      1 Grade   <NA>   <NA>
      2     I    18%    17%
      3    II    16%    18%
      4   III    16%    17%

---

    Code
      tbl_svysummary(svy_trial, by = trt, include = grade, percent = letters,
        statistic = ~"{p}%")
    Condition
      Error in `tbl_svysummary()`:
      ! `percent` must be one of "column", "row", or "cell", not "a".

