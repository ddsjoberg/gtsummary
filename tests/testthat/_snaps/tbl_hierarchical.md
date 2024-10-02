# tbl_hierarchical_count(data) works properly

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, hierarchies = trt))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(hierarchies)
      
        # Now:
        data %>% select(all_of(hierarchies))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(include)
      
        # Now:
        data %>% select(all_of(include))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
        **Chemotherapy Treatment** **N = 200**
      1                     Drug A          98
      2                     Drug B         102

---

    Code
      as.data.frame(tbl_hierarchical_count(data = iris, hierarchies = Species))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(hierarchies)
      
        # Now:
        data %>% select(all_of(hierarchies))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(include)
      
        # Now:
        data %>% select(all_of(include))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
        **Species** **N = 150**
      1      setosa          50
      2  versicolor          50
      3   virginica          50

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
      as.data.frame(tbl_hierarchical_count(data = trial, hierarchies = stage, by = trt))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(hierarchies)
      
        # Now:
        data %>% select(all_of(hierarchies))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(include)
      
        # Now:
        data %>% select(all_of(include))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
        **T Stage** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1          T1                   28                    25
      2          T2                   25                    29
      3          T3                   22                    21
      4          T4                   23                    27

---

    Code
      tbl_hierarchical_count(data = trial, hierarchies = stage, by = name)
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
      tbl_hierarchical_count(data = trial, hierarchies = trt, denominator = 10)
    Condition
      Error in `tbl_hierarchical_count()`:
      ! The `denominator` argument must be class <data.frame> or empty, not a number.

# tbl_hierarchical_count(include) works properly

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade),
      include = grade))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(hierarchies)
      
        # Now:
        data %>% select(all_of(hierarchies))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(include)
      
        # Now:
        data %>% select(all_of(include))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(prior_gp)
      
        # Now:
        data %>% select(all_of(prior_gp))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(prior_gp_lvl)
      
        # Now:
        data %>% select(all_of(prior_gp_lvl))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `dplyr::arrange()`.
      i In argument: `..1 = across(c(ord, var_label))`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(ord)
      
        # Now:
        data %>% select(all_of(ord))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
         **T Stage**  \n    **Grade** **N = 200**
      1                            T1        <NA>
      2                             I          17
      3                            II          23
      4                           III          13
      5                            T2        <NA>
      6                             I          18
      7                            II          17
      8                           III          19
      9                            T3        <NA>
      10                            I          18
      11                           II          11
      12                          III          14
      13                           T4        <NA>
      14                            I          15
      15                           II          17
      16                          III          18

---

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade),
      by = trt, include = NULL))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(hierarchies)
      
        # Now:
        data %>% select(all_of(hierarchies))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(include)
      
        # Now:
        data %>% select(all_of(include))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(prior_gp)
      
        # Now:
        data %>% select(all_of(prior_gp))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(prior_gp_lvl)
      
        # Now:
        data %>% select(all_of(prior_gp_lvl))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `dplyr::arrange()`.
      i In argument: `..1 = across(c(ord, var_label))`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(ord)
      
        # Now:
        data %>% select(all_of(ord))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
         **T Stage**  \n    **Grade** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1                            T1                 <NA>                  <NA>
      2                             I                    8                     9
      3                            II                   14                     9
      4                           III                    6                     7
      5                            T2                 <NA>                  <NA>
      6                             I                    8                    10
      7                            II                    8                     9
      8                           III                    9                    10
      9                            T3                 <NA>                  <NA>
      10                            I                   11                     7
      11                           II                    5                     6
      12                          III                    6                     8
      13                           T4                 <NA>                  <NA>
      14                            I                    8                     7
      15                           II                    5                    12
      16                          III                   10                     8

---

    Code
      tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade), include = name)
    Condition
      Error in `tbl_hierarchical_count()`:
      ! Error processing `include` argument.
      ! Can't select columns that don't exist. x Column `name` doesn't exist.
      i Select among columns "stage" and "grade"

# tbl_hierarchical_count(overall_row) works properly

    Code
      as.data.frame(tbl_hierarchical_count(data = trial, hierarchies = trt,
        overall_row = TRUE))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(hierarchies)
      
        # Now:
        data %>% select(all_of(hierarchies))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(include)
      
        # Now:
        data %>% select(all_of(include))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `df_stats = list(...)`.
      Caused by warning:
      ! Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by_cols)
      
        # Now:
        data %>% select(all_of(by_cols))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
        **Chemotherapy Treatment** **N = 200**
      1    Total number of records         200
      2                     Drug A          98
      3                     Drug B         102

---

    Code
      as.data.frame(res)
    Output
        **Chemotherapy Treatment** **I**  \nN = 68 **II**  \nN = 68 **III**  \nN = 64
      1    Total number of records              68               68                64
      2                     Drug A              35               32                31
      3                     Drug B              33               36                33

---

    Code
      tbl_hierarchical_count(data = trial, hierarchies = trt, overall_row = "test")
    Condition
      Error in `tbl_hierarchical_count()`:
      ! The `overall_row` argument must be class <logical>, not a string.

# tbl_hierarchical_count(label) works properly

    Code
      as.data.frame(res)
    Output
         **My Stage**  \n    **My Grade** **N = 200**
      1                                T1          53
      2                                 I          17
      3                                II          23
      4                               III          13
      5                                T2          54
      6                                 I          18
      7                                II          17
      8                               III          19
      9                                T3          43
      10                                I          18
      11                               II          11
      12                              III          14
      13                               T4          50
      14                                I          15
      15                               II          17
      16                              III          18

---

    "**My Stage**  \n    **My Grade**"

---

    Code
      tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade), label = "Stages")
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(hierarchies)
      
        # Now:
        data %>% select(all_of(hierarchies))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(include)
      
        # Now:
        data %>% select(all_of(include))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Error in `internal_tbl_hierarchical()`:
      ! The `label` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

