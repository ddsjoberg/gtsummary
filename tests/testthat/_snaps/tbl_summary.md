# tbl_summary creates output without error/warning (no by var)

    Code
      purrr::map(lst_tbl, as.data.frame)
    Output
      [[1]]
         **Characteristic**           **N = 32**
      1                 mpg    19.2 (15.4, 22.8)
      2                 cyl                 <NA>
      3                   8             14 (44%)
      4                   4             11 (34%)
      5                   6              7 (22%)
      6                disp       196 (121, 326)
      7                  hp        123 (97, 180)
      8                drat    3.70 (3.08, 3.92)
      9                  wt    3.33 (2.58, 3.61)
      10               qsec 17.71 (16.89, 18.90)
      11                 vs             14 (44%)
      12                 am             13 (41%)
      13               gear                 <NA>
      14                  3             15 (47%)
      15                  4             12 (38%)
      16                  5              5 (16%)
      17               carb                 <NA>
      18                  2             10 (31%)
      19                  4             10 (31%)
      20                  1              7 (22%)
      21                  3             3 (9.4%)
      22                  6             1 (3.1%)
      23                  8             1 (3.1%)
      
      [[2]]
        **Characteristic**       **N = 150**
      1       Sepal.Length 5.80 (5.10, 6.40)
      2        Sepal.Width 3.00 (2.80, 3.30)
      3       Petal.Length 4.35 (1.60, 5.10)
      4        Petal.Width 1.30 (0.30, 1.80)
      5            Species              <NA>
      6             setosa          50 (33%)
      7         versicolor          50 (33%)
      8          virginica          50 (33%)
      

# tbl_summary creates output without error/warning (with by var)

    Code
      tbl_summary(mtcars, by = am) %>% as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13
      1                 mpg    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)
      2                 cyl                 <NA>                 <NA>
      3                   4              3 (16%)              8 (62%)
      4                   6              4 (21%)              3 (23%)
      5                   8             12 (63%)              2 (15%)
      6                disp       276 (196, 360)        120 (79, 160)
      7                  hp       175 (117, 193)        109 (66, 113)
      8                drat    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)
      9                  wt    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)
      10               qsec 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)
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

# tbl_summary allows for named list input

    Code
      tbl_summary(mtcars, by = am, label = list(mpg = "New mpg", cyl = "New cyl")) %>%
        as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13
      1             New mpg    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)
      2             New cyl                 <NA>                 <NA>
      3                   4              3 (16%)              8 (62%)
      4                   6              4 (21%)              3 (23%)
      5                   8             12 (63%)              2 (15%)
      6                disp       276 (196, 360)        120 (79, 160)
      7                  hp       175 (117, 193)        109 (66, 113)
      8                drat    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)
      9                  wt    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)
      10               qsec 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)
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

# tbl_summary value argument works properly

    Code
      tbl_summary(trial, value = "grade" ~ "III") %>% as.data.frame()
    Output
             **Characteristic**       **N = 200**
      1  Chemotherapy Treatment              <NA>
      2                  Drug A          98 (49%)
      3                  Drug B         102 (51%)
      4                     Age       47 (38, 57)
      5                 Unknown                11
      6    Marker Level (ng/mL) 0.64 (0.22, 1.39)
      7                 Unknown                10
      8                 T Stage              <NA>
      9                      T1          53 (27%)
      10                     T2          54 (27%)
      11                     T3          43 (22%)
      12                     T4          50 (25%)
      13                  Grade          64 (32%)
      14         Tumor Response          61 (32%)
      15                Unknown                 7
      16           Patient Died         112 (56%)
      17 Months to Death/Censor 22.4 (16.0, 24.0)

# tbl_summary works in character inputs for `by=`

    Code
      tbl_summary(trial, by = all_of(my_by_variable)) %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age        46 (37, 59)         48 (39, 56)
      2                 Unknown                  7                   4
      3    Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)
      4                 Unknown                  6                   4
      5                 T Stage               <NA>                <NA>
      6                      T1           28 (29%)            25 (25%)
      7                      T2           25 (26%)            29 (28%)
      8                      T3           22 (22%)            21 (21%)
      9                      T4           23 (23%)            27 (26%)
      10                  Grade               <NA>                <NA>
      11                      I           35 (36%)            33 (32%)
      12                     II           32 (33%)            36 (35%)
      13                    III           31 (32%)            33 (32%)
      14         Tumor Response           28 (29%)            33 (34%)
      15                Unknown                  3                   4
      16           Patient Died           52 (53%)            60 (59%)
      17 Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)

---

    Code
      tbl_summary(trial, by = "trt") %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age        46 (37, 59)         48 (39, 56)
      2                 Unknown                  7                   4
      3    Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)
      4                 Unknown                  6                   4
      5                 T Stage               <NA>                <NA>
      6                      T1           28 (29%)            25 (25%)
      7                      T2           25 (26%)            29 (28%)
      8                      T3           22 (22%)            21 (21%)
      9                      T4           23 (23%)            27 (26%)
      10                  Grade               <NA>                <NA>
      11                      I           35 (36%)            33 (32%)
      12                     II           32 (33%)            36 (35%)
      13                    III           31 (32%)            33 (32%)
      14         Tumor Response           28 (29%)            33 (34%)
      15                Unknown                  3                   4
      16           Patient Died           52 (53%)            60 (59%)
      17 Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)

---

    Code
      purrr::map(c("trt", "grade", "stage"), ~ tbl_summary(trial, by = all_of(.x)) %>%
        as_tibble())
    Output
      [[1]]
      # A tibble: 17 x 3
         `**Characteristic**`   `**Drug A**, N = 98` `**Drug B**, N = 102`
         <chr>                  <chr>                <chr>                
       1 Age                    46 (37, 59)          48 (39, 56)          
       2 Unknown                7                    4                    
       3 Marker Level (ng/mL)   0.84 (0.24, 1.57)    0.52 (0.19, 1.20)    
       4 Unknown                6                    4                    
       5 T Stage                <NA>                 <NA>                 
       6 T1                     28 (29%)             25 (25%)             
       7 T2                     25 (26%)             29 (28%)             
       8 T3                     22 (22%)             21 (21%)             
       9 T4                     23 (23%)             27 (26%)             
      10 Grade                  <NA>                 <NA>                 
      11 I                      35 (36%)             33 (32%)             
      12 II                     32 (33%)             36 (35%)             
      13 III                    31 (32%)             33 (32%)             
      14 Tumor Response         28 (29%)             33 (34%)             
      15 Unknown                3                    4                    
      16 Patient Died           52 (53%)             60 (59%)             
      17 Months to Death/Censor 23.5 (17.4, 24.0)    21.2 (14.6, 24.0)    
      
      [[2]]
      # A tibble: 16 x 4
         `**Characteristic**`   `**I**, N = 68`   `**II**, N = 68`  `**III**, N = 64`
         <chr>                  <chr>             <chr>             <chr>            
       1 Chemotherapy Treatment <NA>              <NA>              <NA>             
       2 Drug A                 35 (51%)          32 (47%)          31 (48%)         
       3 Drug B                 33 (49%)          36 (53%)          33 (52%)         
       4 Age                    47 (37, 56)       49 (37, 57)       47 (38, 58)      
       5 Unknown                2                 6                 3                
       6 Marker Level (ng/mL)   1.01 (0.26, 1.61) 0.37 (0.14, 1.11) 0.62 (0.29, 1.68)
       7 Unknown                2                 5                 3                
       8 T Stage                <NA>              <NA>              <NA>             
       9 T1                     17 (25%)          23 (34%)          13 (20%)         
      10 T2                     18 (26%)          17 (25%)          19 (30%)         
      11 T3                     18 (26%)          11 (16%)          14 (22%)         
      12 T4                     15 (22%)          17 (25%)          18 (28%)         
      13 Tumor Response         21 (31%)          19 (30%)          21 (33%)         
      14 Unknown                1                 5                 1                
      15 Patient Died           33 (49%)          36 (53%)          43 (67%)         
      16 Months to Death/Censor 24.0 (18.2, 24.0) 22.2 (13.1, 24.0) 19.7 (16.1, 24.0)
      
      [[3]]
      # A tibble: 15 x 5
         `**Characteristic**`   `**T1**, N = 53`  `**T2**, N = 54`  `**T3**, N = 43` 
         <chr>                  <chr>             <chr>             <chr>            
       1 Chemotherapy Treatment <NA>              <NA>              <NA>             
       2 Drug A                 28 (53%)          25 (46%)          22 (51%)         
       3 Drug B                 25 (47%)          29 (54%)          21 (49%)         
       4 Age                    45 (37, 57)       48 (42, 55)       50 (39, 60)      
       5 Unknown                2                 1                 2                
       6 Marker Level (ng/mL)   0.40 (0.22, 0.98) 0.72 (0.16, 1.87) 1.06 (0.28, 1.56)
       7 Unknown                4                 1                 4                
       8 Grade                  <NA>              <NA>              <NA>             
       9 I                      17 (32%)          18 (33%)          18 (42%)         
      10 II                     23 (43%)          17 (31%)          11 (26%)         
      11 III                    13 (25%)          19 (35%)          14 (33%)         
      12 Tumor Response         18 (35%)          13 (25%)          15 (38%)         
      13 Unknown                1                 2                 3                
      14 Patient Died           24 (45%)          27 (50%)          22 (51%)         
      15 Months to Death/Censor 24.0 (18.2, 24.0) 23.9 (16.5, 24.0) 22.9 (17.0, 24.0)
      # i 1 more variable: `**T4**, N = 50` <chr>
      

# tbl_summary-testing tidyselect parsing

    Code
      big_test %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1  Chemotherapy Treatment               <NA>                <NA>
      2                  Drug A          98 (100%)              0 (0%)
      3                  Drug B             0 (0%)          102 (100%)
      4             Patient Age        6.00 78.000         9.00 83.000
      5    Marker Level (ng/mL)         0.00 3.874          0.01 3.642
      6           Patient Stage                 28                  25
      7                   Grade               <NA>                <NA>
      8                       I                 35                  33
      9                      II                 32                  36
      10                    III                 31                  33
      11         Tumor Response               <NA>                <NA>
      12                      0           67 (71%)            65 (66%)
      13                      1           28 (29%)            33 (34%)
      14           Patient Died               <NA>                <NA>
      15                      0           46 (47%)            42 (41%)
      16                      1           52 (53%)            60 (59%)
      17 Months to Death/Censor           3.5 24.0            5.3 24.0
      18            Crazy Grade           31 (32%)            33 (32%)

# tbl_summary-difftime does not cause error

    Code
      df_dplyr_storms %>% dplyr::mutate(date = ISOdate(year, month, day), date_diff = difftime(
        dplyr::lag(date, 5), date, units = "days")) %>% tbl_summary() %>%
        as.data.frame()
    Output
                   **Characteristic**  **N = 10**
      1                          name        <NA>
      2                           Amy   10 (100%)
      3                          year        <NA>
      4                          1975   10 (100%)
      5                         month        <NA>
      6                             6   10 (100%)
      7                           day        <NA>
      8                            27     4 (40%)
      9                            28     4 (40%)
      10                           29     2 (20%)
      11                         hour        <NA>
      12                            0     3 (30%)
      13                            6     3 (30%)
      14                           12     2 (20%)
      15                           18     2 (20%)
      16                          lat        <NA>
      17                         27.5     1 (10%)
      18                         28.5     1 (10%)
      19                         29.5     1 (10%)
      20                         30.5     1 (10%)
      21                         31.5     1 (10%)
      22                         32.4     1 (10%)
      23                         33.3     1 (10%)
      24                           34     2 (20%)
      25                         34.4     1 (10%)
      26                         long        <NA>
      27                          -79     4 (40%)
      28                        -78.8     1 (10%)
      29                        -78.7     1 (10%)
      30                          -78     1 (10%)
      31                          -77     1 (10%)
      32                        -75.8     1 (10%)
      33                        -74.8     1 (10%)
      34                       status        <NA>
      35                  disturbance      0 (0%)
      36                extratropical      0 (0%)
      37                    hurricane      0 (0%)
      38                    other low      0 (0%)
      39       subtropical depression      0 (0%)
      40            subtropical storm      0 (0%)
      41          tropical depression     8 (80%)
      42               tropical storm     2 (20%)
      43                tropical wave      0 (0%)
      44                     category NA (NA, NA)
      45                      Unknown          10
      46                         wind        <NA>
      47                           25     7 (70%)
      48                           30     1 (10%)
      49                           35     1 (10%)
      50                           40     1 (10%)
      51                     pressure        <NA>
      52                         1002     1 (10%)
      53                         1004     1 (10%)
      54                         1006     1 (10%)
      55                         1011     1 (10%)
      56                         1012     2 (20%)
      57                         1013     4 (40%)
      58 tropicalstorm_force_diameter NA (NA, NA)
      59                      Unknown          10
      60     hurricane_force_diameter NA (NA, NA)
      61                      Unknown          10
      62                         date        <NA>
      63          1975-06-27 12:00:00     4 (40%)
      64          1975-06-28 12:00:00     4 (40%)
      65          1975-06-29 12:00:00     2 (20%)
      66                    date_diff        <NA>
      67                           -2     1 (20%)
      68                           -1     4 (80%)
      69                      Unknown           5

# tbl_summary-all missing data does not cause error

    Code
      all_missing_no_by %>% as.data.frame()
    Output
         **Characteristic**   **N = 4**
      1                 fct        <NA>
      2                lion     0 (NA%)
      3               tiger     0 (NA%)
      4                bear     0 (NA%)
      5             Unknown           4
      6                 lgl     0 (NA%)
      7             Unknown           4
      8                 chr     0 (NA%)
      9             Unknown           4
      10                int NA (NA, NA)
      11            Unknown           4
      12                dbl NA (NA, NA)
      13            Unknown           4

---

    Code
      all_missing_by %>% as.data.frame()
    Output
         **Characteristic** **1**, N = 2 **2**, N = 2
      1                 fct         <NA>         <NA>
      2                lion      0 (NA%)      0 (NA%)
      3               tiger      0 (NA%)      0 (NA%)
      4                bear      0 (NA%)      0 (NA%)
      5             Unknown            2            2
      6                 lgl      0 (NA%)      0 (NA%)
      7             Unknown            2            2
      8                 chr      0 (NA%)      0 (NA%)
      9             Unknown            2            2
      10                int  NA (NA, NA)  NA (NA, NA)
      11            Unknown            2            2
      12                dbl  NA (NA, NA)  NA (NA, NA)
      13            Unknown            2            2

---

    Code
      tbl_summary(df_missing, by = my_by_var, type = vars(int, dbl) ~ "categorical") %>%
        as.data.frame()
    Message
      ! Use of `vars()` is now deprecated and support will soon be removed. Please replace calls to `vars()` with `c()`.
      Variable 'int' is `NA` for all observations and cannot be summarized as
      'categorical'. Using `int ~ "dichotomous"` instead.
      Variable 'dbl' is `NA` for all observations and cannot be summarized as
      'categorical'. Using `dbl ~ "dichotomous"` instead.
    Output
         **Characteristic** **1**, N = 2 **2**, N = 2
      1                 fct         <NA>         <NA>
      2                lion      0 (NA%)      0 (NA%)
      3               tiger      0 (NA%)      0 (NA%)
      4                bear      0 (NA%)      0 (NA%)
      5             Unknown            2            2
      6                 lgl      0 (NA%)      0 (NA%)
      7             Unknown            2            2
      8                 chr      0 (NA%)      0 (NA%)
      9             Unknown            2            2
      10                int      0 (NA%)      0 (NA%)
      11            Unknown            2            2
      12                dbl      0 (NA%)      0 (NA%)
      13            Unknown            2            2

---

    Code
      missing_fct_by %>% as.data.frame()
    Output
             **Characteristic**    **0**, N = 132     **1**, N = 61
      1  Chemotherapy Treatment              <NA>              <NA>
      2                  Drug A          67 (51%)          28 (46%)
      3                  Drug B          65 (49%)          33 (54%)
      4                     Age       46 (36, 55)       49 (43, 59)
      5                 Unknown                 7                 3
      6    Marker Level (ng/mL) 0.59 (0.21, 1.24) 0.98 (0.31, 1.53)
      7                 Unknown                 6                 4
      8                 T Stage              <NA>              <NA>
      9                      T1          34 (26%)          18 (30%)
      10                     T2          39 (30%)          13 (21%)
      11                     T3          25 (19%)          15 (25%)
      12                     T4          34 (26%)          15 (25%)
      13                  Grade              <NA>              <NA>
      14                      I          46 (35%)          21 (34%)
      15                     II          44 (33%)          19 (31%)
      16                    III          42 (32%)          21 (34%)
      17         Tumor Response            0 (0%)         61 (100%)
      18           Patient Died          83 (63%)          24 (39%)
      19 Months to Death/Censor 20.6 (15.0, 24.0) 24.0 (18.4, 24.0)
         **(Missing)**, N = 0
      1                  <NA>
      2               0 (NA%)
      3               0 (NA%)
      4           NA (NA, NA)
      5                     0
      6           NA (NA, NA)
      7                     0
      8                  <NA>
      9               0 (NA%)
      10              0 (NA%)
      11              0 (NA%)
      12              0 (NA%)
      13                 <NA>
      14              0 (NA%)
      15              0 (NA%)
      16              0 (NA%)
      17              0 (NA%)
      18              0 (NA%)
      19          NA (NA, NA)

# tbl_summary-no error when *data frame* with single column passed

    Code
      trial["trt"] %>% as.data.frame() %>% tbl_summary(label = trt ~
      "TREATMENT GROUP") %>% as.data.frame()
    Output
        **Characteristic** **N = 200**
      1    TREATMENT GROUP        <NA>
      2             Drug A    98 (49%)
      3             Drug B   102 (51%)

# tbl_summary-no error when by variable is ordered factor

    Code
      trial %>% dplyr::mutate(grade = as.ordered(grade)) %>% tbl_summary(by = grade) %>%
        as.data.frame()
    Output
             **Characteristic**     **I**, N = 68    **II**, N = 68   **III**, N = 64
      1  Chemotherapy Treatment              <NA>              <NA>              <NA>
      2                  Drug A          35 (51%)          32 (47%)          31 (48%)
      3                  Drug B          33 (49%)          36 (53%)          33 (52%)
      4                     Age       47 (37, 56)       49 (37, 57)       47 (38, 58)
      5                 Unknown                 2                 6                 3
      6    Marker Level (ng/mL) 1.01 (0.26, 1.61) 0.37 (0.14, 1.11) 0.62 (0.29, 1.68)
      7                 Unknown                 2                 5                 3
      8                 T Stage              <NA>              <NA>              <NA>
      9                      T1          17 (25%)          23 (34%)          13 (20%)
      10                     T2          18 (26%)          17 (25%)          19 (30%)
      11                     T3          18 (26%)          11 (16%)          14 (22%)
      12                     T4          15 (22%)          17 (25%)          18 (28%)
      13         Tumor Response          21 (31%)          19 (30%)          21 (33%)
      14                Unknown                 1                 5                 1
      15           Patient Died          33 (49%)          36 (53%)          43 (67%)
      16 Months to Death/Censor 24.0 (18.2, 24.0) 22.2 (13.1, 24.0) 19.7 (16.1, 24.0)

# tbl_summary- works with grouped data (it ungroups it first)

    Code
      trial %>% dplyr::group_by(response) %>% dplyr::select(response, death, trt) %>%
        tbl_summary(by = trt) %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1     Tumor Response           28 (29%)            33 (34%)
      2            Unknown                  3                   4
      3       Patient Died           52 (53%)            60 (59%)

# tbl_summary-works with ordered factors

    Code
      trial %>% select(response, trt) %>% dplyr::mutate_at(vars(response, trt),
      ~ factor(., ordered = TRUE)) %>% tbl_summary(by = trt) %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1           response               <NA>                <NA>
      2                  0           67 (71%)            65 (66%)
      3                  1           28 (29%)            33 (34%)
      4            Unknown                  3                   4

# tbl_summary creates output without error/warning for continuous2 (no by var)

    Code
      purrr::map(list(mtcars, iris), ~ tbl_summary(.x, type = all_continuous() ~
        "continuous2", sort = list(all_categorical() ~ "frequency")) %>% as_tibble())
    Output
      [[1]]
      # A tibble: 29 x 2
         `**Characteristic**` `**N = 32**`     
         <chr>                <chr>            
       1 mpg                  <NA>             
       2 Median (IQR)         19.2 (15.4, 22.8)
       3 cyl                  <NA>             
       4 8                    14 (44%)         
       5 4                    11 (34%)         
       6 6                    7 (22%)          
       7 disp                 <NA>             
       8 Median (IQR)         196 (121, 326)   
       9 hp                   <NA>             
      10 Median (IQR)         123 (97, 180)    
      # i 19 more rows
      
      [[2]]
      # A tibble: 12 x 2
         `**Characteristic**` `**N = 150**`    
         <chr>                <chr>            
       1 Sepal.Length         <NA>             
       2 Median (IQR)         5.80 (5.10, 6.40)
       3 Sepal.Width          <NA>             
       4 Median (IQR)         3.00 (2.80, 3.30)
       5 Petal.Length         <NA>             
       6 Median (IQR)         4.35 (1.60, 5.10)
       7 Petal.Width          <NA>             
       8 Median (IQR)         1.30 (0.30, 1.80)
       9 Species              <NA>             
      10 setosa               50 (33%)         
      11 versicolor           50 (33%)         
      12 virginica            50 (33%)         
      

# tbl_summary creates output without error/warning for continuous2 (with by var)

    Code
      tbl_summary(mtcars, by = am, type = all_continuous() ~ "continuous2") %>%
        as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13
      1                 mpg                 <NA>                 <NA>
      2        Median (IQR)    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)
      3                 cyl                 <NA>                 <NA>
      4                   4              3 (16%)              8 (62%)
      5                   6              4 (21%)              3 (23%)
      6                   8             12 (63%)              2 (15%)
      7                disp                 <NA>                 <NA>
      8        Median (IQR)       276 (196, 360)        120 (79, 160)
      9                  hp                 <NA>                 <NA>
      10       Median (IQR)       175 (117, 193)        109 (66, 113)
      11               drat                 <NA>                 <NA>
      12       Median (IQR)    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)
      13                 wt                 <NA>                 <NA>
      14       Median (IQR)    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)
      15               qsec                 <NA>                 <NA>
      16       Median (IQR) 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)
      17                 vs              7 (37%)              7 (54%)
      18               gear                 <NA>                 <NA>
      19                  3             15 (79%)               0 (0%)
      20                  4              4 (21%)              8 (62%)
      21                  5               0 (0%)              5 (38%)
      22               carb                 <NA>                 <NA>
      23                  1              3 (16%)              4 (31%)
      24                  2              6 (32%)              4 (31%)
      25                  3              3 (16%)               0 (0%)
      26                  4              7 (37%)              3 (23%)
      27                  6               0 (0%)             1 (7.7%)
      28                  8               0 (0%)             1 (7.7%)

# tbl_summary(digits=) tests with fn inputs

    Code
      tbl_digits %>% as.data.frame()
    Output
          **Characteristic**                    **N = 200**
      1                  Age                        4.7e+01
      2 Marker Level (ng/mL)            1 0.86 200.0 95.00%
      3                Grade                           <NA>
      4                    I                   68.0 (34.0%)
      5                   II                   68.0 (34.0%)
      6                  III                   64.0 (32.0%)
      7       Tumor Response 61 193.0 31.61% 200.0 96.5000%

# tbl_summary() continuous vars with cat summary vars only

    Code
      tbl1 %>% as.data.frame()
    Output
        **Characteristic** **N = 200**
      1                Age         200
      2            Unknown          11

---

    Code
      tbl2 %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age                 98                 102
      2            Unknown                  7                   4

# tbl_summary() works with date and date/time

    Code
      tbl1 %>% as.data.frame()
    Output
        **Characteristic**                                 **N = 10**
      1              dates                   2021-02-21 to 2021-03-02
      2              times 2021-02-20 20:31:34 to 2021-02-20 20:31:43
      3              group                                    5 (50%)

---

    Code
      tbl1 %>% as.data.frame()
    Output
        **Characteristic**                     **N = 10**
      1              dates    February 2021 to March 2021
      2              times February 2021 to February 2021

---

    Code
      tbl1 %>% as.data.frame()
    Output
        **Characteristic**                   **0**, N = 5
      1              dates    February 2021 to March 2021
      2              times February 2021 to February 2021
                          **1**, N = 5
      1    February 2021 to March 2021
      2 February 2021 to February 2021

---

    Code
      tbl2 %>% as.data.frame()
    Output
        **Characteristic**                               **0**, N = 5
      1              dates                   2021-02-22 to 2021-03-02
      2              times 2021-02-20 20:31:35 to 2021-02-20 20:31:43
                                      **1**, N = 5
      1                   2021-02-21 to 2021-03-01
      2 2021-02-20 20:31:34 to 2021-02-20 20:31:42

# no error when by variable omitted from include

    Code
      trial %>% tbl_summary(by = trt, include = age) %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2            Unknown                  7                   4

# all column names are accepted

    Code
      tbl_summary(df, by = "variable") %>% as.data.frame()
    Output
        **Characteristic** **A**, N = 5 **B**, N = 5
      1              value     3 (2, 4)     8 (7, 9)

---

    Code
      tbl_summary(df) %>% as.data.frame()
    Output
        **Characteristic** **N = 10**
      1           variable       <NA>
      2                  A    5 (50%)
      3                  B    5 (50%)
      4              value   6 (3, 8)

---

    Code
      tbl_summary(df %>% dplyr::rename(by = variable)) %>% as.data.frame()
    Output
        **Characteristic** **N = 10**
      1                 by       <NA>
      2                  A    5 (50%)
      3                  B    5 (50%)
      4              value   6 (3, 8)

---

    Code
      tbl_summary(df %>% dplyr::rename(by = variable), by = "by") %>% as.data.frame()
    Output
        **Characteristic** **A**, N = 5 **B**, N = 5
      1              value     3 (2, 4)     8 (7, 9)

# no error with factor variable with all NA and no specifed levels

    Code
      tbl
    Output
      # A tibble: 2 x 3
        label      stat_1  stat_2 
        <chr>      <chr>   <chr>  
      1 has_banana 0 (NA%) 0 (NA%)
      2 Unknown    98      102    

# no error when data frame contains named vector

    Code
      tbl_summary(df, type = list(everything() ~ "continuous")) %>% as.data.frame()
    Output
        **Characteristic**    **N = 5**
      1         swallowing 77 (40, 100)
      2            Unknown            1
      3         salivation 81 (58, 100)
      4            Unknown            1

