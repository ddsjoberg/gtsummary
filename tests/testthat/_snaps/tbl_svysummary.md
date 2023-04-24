# tbl_svysummary creates output without error/warning (no by var)

    Code
      purrr::map(list(d, dc_light), ~ tbl_svysummary(.x, sort = list(all_categorical() ~
        "frequency")) %>% as_tibble())
    Output
      [[1]]
      # A tibble: 13 x 2
         `**Characteristic**` `**N = 2,201**`
         <chr>                <chr>          
       1 Class                <NA>           
       2 1st                  325 (15%)      
       3 2nd                  285 (13%)      
       4 3rd                  706 (32%)      
       5 Crew                 885 (40%)      
       6 Sex                  <NA>           
       7 Male                 1,731 (79%)    
       8 Female               470 (21%)      
       9 Age                  <NA>           
      10 Child                109 (5.0%)     
      11 Adult                2,092 (95%)    
      12 Survived             711 (32%)      
      13 Freq                 183 (92, 438)  
      
      [[2]]
      # A tibble: 6 x 2
        `**Characteristic**` `**N = 6,194**`
        <chr>                <chr>          
      1 stype                <NA>           
      2 E                    4,874 (79%)    
      3 M                    846 (14%)      
      4 H                    474 (7.7%)     
      5 growth               33 (17, 53)    
      6 both                 4,502 (73%)    
      

# tbl_svysummary creates output without error/warning (with by var)

    Code
      tbl_svysummary(dc_light, by = both, statistic = statistics) %>% as_tibble()
    Output
      # A tibble: 5 x 3
        `**Characteristic**` `**No**, N = 1,692`                  `**Yes**, N = 4,502`
        <chr>                <chr>                                <chr>               
      1 stype                <NA>                                 <NA>                
      2 E                    1,083 1,692 64 | 32 50 64 0.09 1.9   3,791 4,502 84 | 11~
      3 H                    237 1,692 14 | 7 50 14 0.05 0.91     237 4,502 5.3 | 7 1~
      4 M                    372 1,692 22 | 11 50 22 0.09 2.2     474 4,502 11 | 14 1~
      5 growth               7 9 17 302 -34 47 14,927 -2 6 19 30~ 44 48 26 689 8 131 ~

# tbl_svysummary allows for named list input

    Code
      tbl_svysummary(d, by = Survived, label = list(Class = "New Class", Sex = "New Sex")) %>%
        as.data.frame()
    Output
         **Characteristic** **No**, N = 1,490 **Yes**, N = 711
      1           New Class              <NA>             <NA>
      2                 1st        122 (8.2%)        203 (29%)
      3                 2nd         167 (11%)        118 (17%)
      4                 3rd         528 (35%)        178 (25%)
      5                Crew         673 (45%)        212 (30%)
      6             New Sex              <NA>             <NA>
      7                Male       1,364 (92%)        367 (52%)
      8              Female        126 (8.5%)        344 (48%)
      9                 Age              <NA>             <NA>
      10              Child         52 (3.5%)        57 (8.0%)
      11              Adult       1,438 (97%)        654 (92%)
      12               Freq    342 (140, 513)     79 (64, 144)

# tbl_svysummary value argument works properly

    Code
      tbl_svysummary(d, value = "Class" ~ "1st") %>% as.data.frame()
    Output
        **Characteristic** **N = 2,201**
      1              Class     325 (15%)
      2                Sex          <NA>
      3               Male   1,731 (79%)
      4             Female     470 (21%)
      5                Age          <NA>
      6              Child    109 (5.0%)
      7              Adult   2,092 (95%)
      8           Survived     711 (32%)
      9               Freq 183 (92, 438)

# tbl_svysummary works in character inputs for `by=`

    Code
      tbl_svysummary(d, by = all_of(my_by_variable)) %>% as.data.frame()
    Output
         **Characteristic** **No**, N = 1,490 **Yes**, N = 711
      1               Class              <NA>             <NA>
      2                 1st        122 (8.2%)        203 (29%)
      3                 2nd         167 (11%)        118 (17%)
      4                 3rd         528 (35%)        178 (25%)
      5                Crew         673 (45%)        212 (30%)
      6                 Sex              <NA>             <NA>
      7                Male       1,364 (92%)        367 (52%)
      8              Female        126 (8.5%)        344 (48%)
      9                 Age              <NA>             <NA>
      10              Child         52 (3.5%)        57 (8.0%)
      11              Adult       1,438 (97%)        654 (92%)
      12               Freq    342 (140, 513)     79 (64, 144)

---

    Code
      tbl_svysummary(d, by = "Survived") %>% as.data.frame()
    Output
         **Characteristic** **No**, N = 1,490 **Yes**, N = 711
      1               Class              <NA>             <NA>
      2                 1st        122 (8.2%)        203 (29%)
      3                 2nd         167 (11%)        118 (17%)
      4                 3rd         528 (35%)        178 (25%)
      5                Crew         673 (45%)        212 (30%)
      6                 Sex              <NA>             <NA>
      7                Male       1,364 (92%)        367 (52%)
      8              Female        126 (8.5%)        344 (48%)
      9                 Age              <NA>             <NA>
      10              Child         52 (3.5%)        57 (8.0%)
      11              Adult       1,438 (97%)        654 (92%)
      12               Freq    342 (140, 513)     79 (64, 144)

---

    Code
      purrr::map(c("Survived", "Class", "Sex", "Age"), ~ tbl_svysummary(d, by = all_of(
        .x)) %>% as_tibble())
    Output
      [[1]]
      # A tibble: 12 x 3
         `**Characteristic**` `**No**, N = 1,490` `**Yes**, N = 711`
         <chr>                <chr>               <chr>             
       1 Class                <NA>                <NA>              
       2 1st                  122 (8.2%)          203 (29%)         
       3 2nd                  167 (11%)           118 (17%)         
       4 3rd                  528 (35%)           178 (25%)         
       5 Crew                 673 (45%)           212 (30%)         
       6 Sex                  <NA>                <NA>              
       7 Male                 1,364 (92%)         367 (52%)         
       8 Female               126 (8.5%)          344 (48%)         
       9 Age                  <NA>                <NA>              
      10 Child                52 (3.5%)           57 (8.0%)         
      11 Adult                1,438 (97%)         654 (92%)         
      12 Freq                 342 (140, 513)      79 (64, 144)      
      
      [[2]]
      # A tibble: 8 x 5
        `**Characteristic**` `**1st**, N = 325` `**2nd**, N = 285` `**3rd**, N = 706`
        <chr>                <chr>              <chr>              <chr>             
      1 Sex                  <NA>               <NA>               <NA>              
      2 Male                 180 (55%)          179 (63%)          510 (72%)         
      3 Female               145 (45%)          106 (37%)          196 (28%)         
      4 Age                  <NA>               <NA>               <NA>              
      5 Child                6 (1.8%)           24 (8.4%)          79 (11%)          
      6 Adult                319 (98%)          261 (92%)          627 (89%)         
      7 Survived             203 (62%)          118 (41%)          178 (25%)         
      8 Freq                 106 (64, 127)      86 (31, 120)       115 (75, 251)     
      # i 1 more variable: `**Crew**, N = 885` <chr>
      
      [[3]]
      # A tibble: 10 x 3
         `**Characteristic**` `**Male**, N = 1,731` `**Female**, N = 470`
         <chr>                <chr>                 <chr>                
       1 Class                <NA>                  <NA>                 
       2 1st                  180 (10%)             145 (31%)            
       3 2nd                  179 (10%)             106 (23%)            
       4 3rd                  510 (29%)             196 (42%)            
       5 Crew                 862 (50%)             23 (4.9%)            
       6 Age                  <NA>                  <NA>                 
       7 Child                64 (3.7%)             45 (9.6%)            
       8 Adult                1,667 (96%)           425 (90%)            
       9 Survived             367 (21%)             344 (73%)            
      10 Freq                 288 (142, 487)        80 (44, 97)          
      
      [[4]]
      # A tibble: 10 x 3
         `**Characteristic**` `**Child**, N = 109` `**Adult**, N = 2,092`
         <chr>                <chr>                <chr>                 
       1 Class                <NA>                 <NA>                  
       2 1st                  6 (5.5%)             319 (15%)             
       3 2nd                  24 (22%)             261 (12%)             
       4 3rd                  79 (72%)             627 (30%)             
       5 Crew                 0 (0%)               885 (42%)             
       6 Sex                  <NA>                 <NA>                  
       7 Male                 64 (59%)             1,667 (80%)           
       8 Female               45 (41%)             425 (20%)             
       9 Survived             57 (52%)             654 (31%)             
      10 Freq                 14 (13, 21)          198 (112, 449)        
      

# tbl_svysummary-testing tidyselect parsing

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

# tbl_svysummary-difftime does not cause error

    Code
      df_dplyr_storms %>% dplyr::mutate(date = ISOdate(year, month, day), date_diff = difftime(
        dplyr::lag(date, 5), date, units = "days")) %>% survey::svydesign(data = .,
        ids = ~1, weights = ~1) %>% tbl_svysummary() %>% as_tibble()
    Output
      # A tibble: 69 x 2
         `**Characteristic**` `**N = 10**`
         <chr>                <chr>       
       1 name                 <NA>        
       2 Amy                  10 (100%)   
       3 year                 <NA>        
       4 1975                 10 (100%)   
       5 month                <NA>        
       6 6                    10 (100%)   
       7 day                  <NA>        
       8 27                   4 (40%)     
       9 28                   4 (40%)     
      10 29                   2 (20%)     
      # i 59 more rows

# tbl_svysummary-all missing data does not cause error

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
      tbl_svysummary(design_missing, by = my_by_var, type = c(int, dbl) ~
        "categorical") %>% as.data.frame()
    Message
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

# tbl_svysummary-no error when *data* with single column passed

    Code
      trial["trt"] %>% as.data.frame() %>% survey::svydesign(data = ., ids = ~1,
        weights = ~1) %>% tbl_svysummary(label = trt ~ "TREATMENT GROUP") %>%
        as.data.frame()
    Output
        **Characteristic** **N = 200**
      1    TREATMENT GROUP        <NA>
      2             Drug A    98 (49%)
      3             Drug B   102 (51%)

# tbl_svysummary-no error when by variable is ordered factor

    Code
      trial %>% dplyr::mutate(grade = as.ordered(grade)) %>% survey::svydesign(data = .,
        ids = ~1, weights = ~1) %>% tbl_svysummary(by = grade) %>% as.data.frame()
    Output
             **Characteristic**     **I**, N = 68    **II**, N = 68   **III**, N = 64
      1  Chemotherapy Treatment              <NA>              <NA>              <NA>
      2                  Drug A          35 (51%)          32 (47%)          31 (48%)
      3                  Drug B          33 (49%)          36 (53%)          33 (52%)
      4                     Age       47 (37, 56)       48 (36, 57)       47 (38, 58)
      5                 Unknown                 2                 6                 3
      6    Marker Level (ng/mL) 0.98 (0.24, 1.58) 0.37 (0.14, 1.09) 0.61 (0.26, 1.67)
      7                 Unknown                 2                 5                 3
      8                 T Stage              <NA>              <NA>              <NA>
      9                      T1          17 (25%)          23 (34%)          13 (20%)
      10                     T2          18 (26%)          17 (25%)          19 (30%)
      11                     T3          18 (26%)          11 (16%)          14 (22%)
      12                     T4          15 (22%)          17 (25%)          18 (28%)
      13         Tumor Response          21 (31%)          19 (30%)          21 (33%)
      14                Unknown                 1                 5                 1
      15           Patient Died          33 (49%)          36 (53%)          43 (67%)
      16 Months to Death/Censor 24.0 (17.8, 24.0) 21.6 (13.0, 24.0) 19.5 (15.8, 24.0)

# tbl_summary(digits=) tests with fn inputs

    Code
      tbl_digits %>% as.data.frame()
    Output
        **Characteristic** **N = 6,194**
      1               emer       0.00 49

# tbl_svysummary() works with date and date/time

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

# tbl_svysummary() works with 0/1 variables

    Code
      survey::svydesign(data = trial, ids = ~1, weights = ~1) %>% tbl_svysummary(
        include = response) %>% as.data.frame()
    Output
        **Characteristic** **N = 200**
      1     Tumor Response    61 (32%)
      2            Unknown           7

# tbl_svysummary() works with a factor having only one levem

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **N = 5**
      1                fct      <NA>
      2                  a  5 (100%)

