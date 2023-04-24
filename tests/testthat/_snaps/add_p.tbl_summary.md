# add_p creates output without error/warning

    Code
      tbl_summary(trial, by = grade) %>% add_p() %>% as.data.frame()
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
         **p-value**
      1          0.9
      2         <NA>
      3         <NA>
      4          0.8
      5         <NA>
      6        0.019
      7         <NA>
      8          0.6
      9         <NA>
      10        <NA>
      11        <NA>
      12        <NA>
      13        >0.9
      14        <NA>
      15       0.080
      16       0.060

---

    Code
      tbl
    Output
      # A tibble: 22 x 4
         `**Characteristic**` `**0**, N = 19`      `**1**, N = 13`      `**p-value**`
         <chr>                <chr>                <chr>                <chr>        
       1 mpg                  17.3 (15.0, 19.2)    22.8 (21.0, 30.4)    0.002        
       2 cyl                  <NA>                 <NA>                 0.009        
       3 4                    3 (16%)              8 (62%)              <NA>         
       4 6                    4 (21%)              3 (23%)              <NA>         
       5 8                    12 (63%)             2 (15%)              <NA>         
       6 disp                 276 (196, 360)       120 (79, 160)        <0.001       
       7 hp                   175 (117, 193)       109 (66, 113)        0.046        
       8 drat                 3.15 (3.07, 3.70)    4.08 (3.85, 4.22)    <0.001       
       9 wt                   3.52 (3.44, 3.84)    2.32 (1.94, 2.78)    <0.001       
      10 qsec                 17.82 (17.18, 19.17) 17.02 (16.46, 18.61) 0.3          
      # i 12 more rows

---

    Code
      trial %>% tbl_summary(by = trt) %>% add_p() %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                     Age        46 (37, 59)         48 (39, 56)         0.7
      2                 Unknown                  7                   4        <NA>
      3    Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)       0.085
      4                 Unknown                  6                   4        <NA>
      5                 T Stage               <NA>                <NA>         0.9
      6                      T1           28 (29%)            25 (25%)        <NA>
      7                      T2           25 (26%)            29 (28%)        <NA>
      8                      T3           22 (22%)            21 (21%)        <NA>
      9                      T4           23 (23%)            27 (26%)        <NA>
      10                  Grade               <NA>                <NA>         0.9
      11                      I           35 (36%)            33 (32%)        <NA>
      12                     II           32 (33%)            36 (35%)        <NA>
      13                    III           31 (32%)            33 (32%)        <NA>
      14         Tumor Response           28 (29%)            33 (34%)         0.5
      15                Unknown                  3                   4        <NA>
      16           Patient Died           52 (53%)            60 (59%)         0.4
      17 Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)        0.14

---

    Code
      tbl_summary(trial, by = trt, include = -response) %>% add_p(group = response) %>%
        as.data.frame()
    Message
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                     Age        46 (37, 59)         48 (39, 56)        >0.9
      2                 Unknown                  7                   4        <NA>
      3    Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)         0.2
      4                 Unknown                  6                   4        <NA>
      5                 T Stage               <NA>                <NA>         0.9
      6                      T1           28 (29%)            25 (25%)        <NA>
      7                      T2           25 (26%)            29 (28%)        <NA>
      8                      T3           22 (22%)            21 (21%)        <NA>
      9                      T4           23 (23%)            27 (26%)        <NA>
      10                  Grade               <NA>                <NA>         0.8
      11                      I           35 (36%)            33 (32%)        <NA>
      12                     II           32 (33%)            36 (35%)        <NA>
      13                    III           31 (32%)            33 (32%)        <NA>
      14           Patient Died           52 (53%)            60 (59%)         0.3
      15 Months to Death/Censor  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)       0.042

# add_p creates output without error/warning for continuous2

    Code
      tbl_summary(trial, by = grade, type = all_continuous() ~ "continuous2") %>%
        add_p() %>% as_tibble()
    Output
      # A tibble: 19 x 5
         `**Characteristic**`   `**I**, N = 68`   `**II**, N = 68`  `**III**, N = 64`
         <chr>                  <chr>             <chr>             <chr>            
       1 Chemotherapy Treatment <NA>              <NA>              <NA>             
       2 Drug A                 35 (51%)          32 (47%)          31 (48%)         
       3 Drug B                 33 (49%)          36 (53%)          33 (52%)         
       4 Age                    <NA>              <NA>              <NA>             
       5 Median (IQR)           47 (37, 56)       49 (37, 57)       47 (38, 58)      
       6 Unknown                2                 6                 3                
       7 Marker Level (ng/mL)   <NA>              <NA>              <NA>             
       8 Median (IQR)           1.01 (0.26, 1.61) 0.37 (0.14, 1.11) 0.62 (0.29, 1.68)
       9 Unknown                2                 5                 3                
      10 T Stage                <NA>              <NA>              <NA>             
      11 T1                     17 (25%)          23 (34%)          13 (20%)         
      12 T2                     18 (26%)          17 (25%)          19 (30%)         
      13 T3                     18 (26%)          11 (16%)          14 (22%)         
      14 T4                     15 (22%)          17 (25%)          18 (28%)         
      15 Tumor Response         21 (31%)          19 (30%)          21 (33%)         
      16 Unknown                1                 5                 1                
      17 Patient Died           33 (49%)          36 (53%)          43 (67%)         
      18 Months to Death/Censor <NA>              <NA>              <NA>             
      19 Median (IQR)           24.0 (18.2, 24.0) 22.2 (13.1, 24.0) 19.7 (16.1, 24.0)
      # i 1 more variable: `**p-value**` <chr>

---

    Code
      tbl
    Output
      # A tibble: 28 x 4
         `**Characteristic**` `**0**, N = 19`   `**1**, N = 13`   `**p-value**`
         <chr>                <chr>             <chr>             <chr>        
       1 mpg                  <NA>              <NA>              0.002        
       2 Median (IQR)         17.3 (15.0, 19.2) 22.8 (21.0, 30.4) <NA>         
       3 cyl                  <NA>              <NA>              0.009        
       4 4                    3 (16%)           8 (62%)           <NA>         
       5 6                    4 (21%)           3 (23%)           <NA>         
       6 8                    12 (63%)          2 (15%)           <NA>         
       7 disp                 <NA>              <NA>              <0.001       
       8 Median (IQR)         276 (196, 360)    120 (79, 160)     <NA>         
       9 hp                   <NA>              <NA>              0.046        
      10 Median (IQR)         175 (117, 193)    109 (66, 113)     <NA>         
      # i 18 more rows

---

    Code
      trial %>% tbl_summary(by = trt, type = all_continuous() ~ "continuous2") %>%
        add_p() %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                     Age               <NA>                <NA>         0.7
      2            Median (IQR)        46 (37, 59)         48 (39, 56)        <NA>
      3                 Unknown                  7                   4        <NA>
      4    Marker Level (ng/mL)               <NA>                <NA>       0.085
      5            Median (IQR)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)        <NA>
      6                 Unknown                  6                   4        <NA>
      7                 T Stage               <NA>                <NA>         0.9
      8                      T1           28 (29%)            25 (25%)        <NA>
      9                      T2           25 (26%)            29 (28%)        <NA>
      10                     T3           22 (22%)            21 (21%)        <NA>
      11                     T4           23 (23%)            27 (26%)        <NA>
      12                  Grade               <NA>                <NA>         0.9
      13                      I           35 (36%)            33 (32%)        <NA>
      14                     II           32 (33%)            36 (35%)        <NA>
      15                    III           31 (32%)            33 (32%)        <NA>
      16         Tumor Response           28 (29%)            33 (34%)         0.5
      17                Unknown                  3                   4        <NA>
      18           Patient Died           52 (53%)            60 (59%)         0.4
      19 Months to Death/Censor               <NA>                <NA>        0.14
      20           Median (IQR)  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)        <NA>

---

    Code
      tbl_summary(trial, by = trt, include = -response, type = all_continuous() ~
        "continuous2") %>% add_p(group = response) %>% as.data.frame()
    Message
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
      boundary (singular) fit: see help('isSingular')
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                     Age               <NA>                <NA>        >0.9
      2            Median (IQR)        46 (37, 59)         48 (39, 56)        <NA>
      3                 Unknown                  7                   4        <NA>
      4    Marker Level (ng/mL)               <NA>                <NA>         0.2
      5            Median (IQR)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)        <NA>
      6                 Unknown                  6                   4        <NA>
      7                 T Stage               <NA>                <NA>         0.9
      8                      T1           28 (29%)            25 (25%)        <NA>
      9                      T2           25 (26%)            29 (28%)        <NA>
      10                     T3           22 (22%)            21 (21%)        <NA>
      11                     T4           23 (23%)            27 (26%)        <NA>
      12                  Grade               <NA>                <NA>         0.8
      13                      I           35 (36%)            33 (32%)        <NA>
      14                     II           32 (33%)            36 (35%)        <NA>
      15                    III           31 (32%)            33 (32%)        <NA>
      16           Patient Died           52 (53%)            60 (59%)         0.3
      17 Months to Death/Censor               <NA>                <NA>       0.042
      18           Median (IQR)  23.5 (17.4, 24.0)   21.2 (14.6, 24.0)        <NA>

# add_p works well

    Code
      tbl
    Output
      # A tibble: 22 x 4
         `**Characteristic**` `**0**, N = 19`      `**1**, N = 13`      `**p-value**`
         <chr>                <chr>                <chr>                <chr>        
       1 mpg                  17.3 (15.0, 19.2)    22.8 (21.0, 30.4)    0.001        
       2 cyl                  <NA>                 <NA>                 0.013        
       3 4                    3 (16%)              8 (62%)              <NA>         
       4 6                    4 (21%)              3 (23%)              <NA>         
       5 8                    12 (63%)             2 (15%)              <NA>         
       6 disp                 276 (196, 360)       120 (79, 160)        <0.001       
       7 hp                   175 (117, 193)       109 (66, 113)        0.2          
       8 drat                 3.15 (3.07, 3.70)    4.08 (3.85, 4.22)    <0.001       
       9 wt                   3.52 (3.44, 3.84)    2.32 (1.94, 2.78)    <0.001       
      10 qsec                 17.82 (17.18, 19.17) 17.02 (16.46, 18.61) 0.3          
      # i 12 more rows

---

    Code
      tbl
    Output
      # A tibble: 22 x 4
         `**Characteristic**` `**0**, N = 19`      `**1**, N = 13`      `**p-value**`
         <chr>                <chr>                <chr>                <chr>        
       1 mpg                  17.3 (15.0, 19.2)    22.8 (21.0, 30.4)    0.001        
       2 cyl                  <NA>                 <NA>                 0.009        
       3 4                    3 (16%)              8 (62%)              <NA>         
       4 6                    4 (21%)              3 (23%)              <NA>         
       5 8                    12 (63%)             2 (15%)              <NA>         
       6 disp                 276 (196, 360)       120 (79, 160)        <0.001       
       7 hp                   175 (117, 193)       109 (66, 113)        0.046        
       8 drat                 3.15 (3.07, 3.70)    4.08 (3.85, 4.22)    <0.001       
       9 wt                   3.52 (3.44, 3.84)    2.32 (1.94, 2.78)    <0.001       
      10 qsec                 17.82 (17.18, 19.17) 17.02 (16.46, 18.61) 0.3          
      # i 12 more rows

# add_p with custom p-value function

    Code
      trial[c("response", "trt")] %>% tbl_summary(by = trt) %>% add_p(test = response ~
        "my_mcnemar") %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1     Tumor Response           28 (29%)            33 (34%)      <0.001
      2            Unknown                  3                   4        <NA>

---

    Code
      trial[c("response", "trt")] %>% tbl_summary(by = trt) %>% add_p(test = response ~
        "my_mcnemar2") %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1     Tumor Response           28 (29%)            33 (34%)      <0.001
      2            Unknown                  3                   4        <NA>

---

    Code
      tbl_mcnemar %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1     Tumor Response           28 (29%)            33 (34%)      <0.001
      2            Unknown                  3                   4        <NA>

# p-values are replicated within tbl_summary()

    Code
      tbl_test.args %>% as.data.frame()
    Output
         **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                 Age        46 (37, 59)         48 (39, 56)         0.8
      2                 Age        46 (37, 59)         48 (39, 56)         0.8
      3                 Age        46 (37, 59)         48 (39, 56)         0.7
      4                 Age        46 (37, 59)         48 (39, 56)         0.7
      5                 Age        46 (37, 59)         48 (39, 56)         0.7
      6                 Age        46 (37, 59)         48 (39, 56)         0.8
      7      Tumor Response           28 (29%)            33 (34%)         0.6
      8      Tumor Response           28 (29%)            33 (34%)         0.5
      9      Tumor Response           28 (29%)            33 (34%)         0.5
      10     Tumor Response           28 (29%)            33 (34%)         0.5
      11     Tumor Response           28 (29%)            33 (34%)         0.3
      12     Tumor Response           28 (29%)            33 (34%)      <0.001
      13     Tumor Response           28 (29%)            33 (34%)      <0.001

---

    Code
      tbl_groups %>% as.data.frame()
    Output
         **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1               Grade               <NA>                <NA>         0.9
      2                   I           35 (36%)            33 (32%)        <NA>
      3                  II           32 (33%)            36 (35%)        <NA>
      4                 III           31 (32%)            33 (32%)        <NA>
      5               Grade               <NA>                <NA>        >0.9
      6                   I           35 (36%)            33 (32%)        <NA>
      7                  II           32 (33%)            36 (35%)        <NA>
      8                 III           31 (32%)            33 (32%)        <NA>
      9      Tumor Response           28 (29%)            33 (34%)         0.5
      10     Tumor Response           28 (29%)            33 (34%)         0.4
      11                Age        46 (37, 59)         48 (39, 56)         0.6
      12                Age        46 (37, 59)         48 (39, 56)         0.3
      13                Age        46 (37, 59)         48 (39, 56)         0.6
      14                Age        46 (37, 59)         48 (39, 56)         0.3

# Groups arg and lme4

    Code
      tbl_groups %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **p-value**
      1                Age        46 (37, 59)         48 (39, 56)         0.8

# no error with missing data

    Code
      t1 %>% as.data.frame()
    Output
        **Characteristic** **0**, N = 19 **1**, N = 13 **p-value**
      1         has_banana          <NA>          <NA>        <NA>
      2                Yes       0 (NA%)       0 (NA%)        <NA>
      3                 No       0 (NA%)       0 (NA%)        <NA>
      4            Unknown            19            13        <NA>
      5                mpg       0 (NA%)       0 (NA%)        <NA>
      6            Unknown            19            13        <NA>
      7                 hp   NA (NA, NA)   NA (NA, NA)        <NA>
      8            Unknown            19            13        <NA>

# add_p can be run after add_difference()

    Code
      tbl
    Output
      # A tibble: 1 x 6
        label stat_1 stat_2 estimate ci          p.value
        <chr> <chr>  <chr>  <chr>    <chr>       <chr>  
      1 Age   47.011 47.449 -0.03    -0.32, 0.25 0.8    

