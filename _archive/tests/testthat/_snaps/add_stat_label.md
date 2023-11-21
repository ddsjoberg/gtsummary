# no errors/warnings with standard use

    Code
      tbl %>% add_stat_label() %>% as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13
      1   mpg, Median (IQR)    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)
      2          cyl, n (%)                 <NA>                 <NA>
      3                   4              3 (16%)              8 (62%)
      4                   6              4 (21%)              3 (23%)
      5                   8             12 (63%)              2 (15%)
      6  disp, Median (IQR)       276 (196, 360)        120 (79, 160)
      7    hp, Median (IQR)       175 (117, 193)        109 (66, 113)
      8  drat, Median (IQR)    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)
      9    wt, Median (IQR)    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)
      10 qsec, Median (IQR) 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)
      11          vs, n (%)              7 (37%)              7 (54%)
      12        gear, n (%)                 <NA>                 <NA>
      13                  3             15 (79%)               0 (0%)
      14                  4              4 (21%)              8 (62%)
      15                  5               0 (0%)              5 (38%)
      16        carb, n (%)                 <NA>                 <NA>
      17                  1              3 (16%)              4 (31%)
      18                  2              6 (32%)              4 (31%)
      19                  3              3 (16%)               0 (0%)
      20                  4              7 (37%)              3 (23%)
      21                  6               0 (0%)             1 (7.7%)
      22                  8               0 (0%)             1 (7.7%)

---

    Code
      tbl00 %>% as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13 **p-value**
      1   mpg, Median (IQR)    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)       0.002
      2          cyl, n (%)                 <NA>                 <NA>       0.009
      3                   4              3 (16%)              8 (62%)        <NA>
      4                   6              4 (21%)              3 (23%)        <NA>
      5                   8             12 (63%)              2 (15%)        <NA>
      6  disp, Median (IQR)       276 (196, 360)        120 (79, 160)      <0.001
      7    hp, Median (IQR)       175 (117, 193)        109 (66, 113)       0.046
      8  drat, Median (IQR)    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)      <0.001
      9    wt, Median (IQR)    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)      <0.001
      10 qsec, Median (IQR) 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)         0.3
      11          vs, n (%)              7 (37%)              7 (54%)         0.3
      12        gear, n (%)                 <NA>                 <NA>      <0.001
      13                  3             15 (79%)               0 (0%)        <NA>
      14                  4              4 (21%)              8 (62%)        <NA>
      15                  5               0 (0%)              5 (38%)        <NA>
      16        carb, n (%)                 <NA>                 <NA>         0.3
      17                  1              3 (16%)              4 (31%)        <NA>
      18                  2              6 (32%)              4 (31%)        <NA>
      19                  3              3 (16%)               0 (0%)        <NA>
      20                  4              7 (37%)              3 (23%)        <NA>
      21                  6               0 (0%)             1 (7.7%)        <NA>
      22                  8               0 (0%)             1 (7.7%)        <NA>

---

    Code
      tbl %>% add_overall() %>% add_stat_label() %>% as.data.frame()
    Output
         **Characteristic**  **Overall**, N = 32        **0**, N = 19
      1   mpg, Median (IQR)    19.2 (15.4, 22.8)    17.3 (15.0, 19.2)
      2          cyl, n (%)                 <NA>                 <NA>
      3                   4             11 (34%)              3 (16%)
      4                   6              7 (22%)              4 (21%)
      5                   8             14 (44%)             12 (63%)
      6  disp, Median (IQR)       196 (121, 326)       276 (196, 360)
      7    hp, Median (IQR)        123 (97, 180)       175 (117, 193)
      8  drat, Median (IQR)    3.70 (3.08, 3.92)    3.15 (3.07, 3.70)
      9    wt, Median (IQR)    3.33 (2.58, 3.61)    3.52 (3.44, 3.84)
      10 qsec, Median (IQR) 17.71 (16.89, 18.90) 17.82 (17.18, 19.17)
      11          vs, n (%)             14 (44%)              7 (37%)
      12        gear, n (%)                 <NA>                 <NA>
      13                  3             15 (47%)             15 (79%)
      14                  4             12 (38%)              4 (21%)
      15                  5              5 (16%)               0 (0%)
      16        carb, n (%)                 <NA>                 <NA>
      17                  1              7 (22%)              3 (16%)
      18                  2             10 (31%)              6 (32%)
      19                  3             3 (9.4%)              3 (16%)
      20                  4             10 (31%)              7 (37%)
      21                  6             1 (3.1%)               0 (0%)
      22                  8             1 (3.1%)               0 (0%)
                **1**, N = 13
      1     22.8 (21.0, 30.4)
      2                  <NA>
      3               8 (62%)
      4               3 (23%)
      5               2 (15%)
      6         120 (79, 160)
      7         109 (66, 113)
      8     4.08 (3.85, 4.22)
      9     2.32 (1.94, 2.78)
      10 17.02 (16.46, 18.61)
      11              7 (54%)
      12                 <NA>
      13               0 (0%)
      14              8 (62%)
      15              5 (38%)
      16                 <NA>
      17              4 (31%)
      18              4 (31%)
      19               0 (0%)
      20              3 (23%)
      21             1 (7.7%)
      22             1 (7.7%)

---

    Code
      tbl %>% add_stat_label(location = "column", label = all_categorical() ~
        "no. (%)") %>% as.data.frame()
    Output
         **Characteristic** **Statistic**        **0**, N = 19        **1**, N = 13
      1                 mpg  Median (IQR)    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)
      2                 cyl          <NA>                 <NA>                 <NA>
      3                   4       no. (%)              3 (16%)              8 (62%)
      4                   6       no. (%)              4 (21%)              3 (23%)
      5                   8       no. (%)             12 (63%)              2 (15%)
      6                disp  Median (IQR)       276 (196, 360)        120 (79, 160)
      7                  hp  Median (IQR)       175 (117, 193)        109 (66, 113)
      8                drat  Median (IQR)    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)
      9                  wt  Median (IQR)    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)
      10               qsec  Median (IQR) 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)
      11                 vs       no. (%)              7 (37%)              7 (54%)
      12               gear          <NA>                 <NA>                 <NA>
      13                  3       no. (%)             15 (79%)               0 (0%)
      14                  4       no. (%)              4 (21%)              8 (62%)
      15                  5       no. (%)               0 (0%)              5 (38%)
      16               carb          <NA>                 <NA>                 <NA>
      17                  1       no. (%)              3 (16%)              4 (31%)
      18                  2       no. (%)              6 (32%)              4 (31%)
      19                  3       no. (%)              3 (16%)               0 (0%)
      20                  4       no. (%)              7 (37%)              3 (23%)
      21                  6       no. (%)               0 (0%)             1 (7.7%)
      22                  8       no. (%)               0 (0%)             1 (7.7%)

---

    Code
      tbl %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age               <NA>                <NA>
      2          Mean (SD)            47 (15)             47 (14)
      3          Min - Max             6 - 78              9 - 83
      4            Unknown                  7                   4
      5       Grade, n (%)               <NA>                <NA>
      6                  I           35 (36%)            33 (32%)
      7                 II           32 (33%)            36 (35%)
      8                III           31 (32%)            33 (32%)

# no errors/warnings with standard use for continuous2

    Code
      tbl %>% add_stat_label() %>% as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13
      1                 mpg                 <NA>                 <NA>
      2        Median (IQR)    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)
      3          cyl, n (%)                 <NA>                 <NA>
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
      17          vs, n (%)              7 (37%)              7 (54%)
      18        gear, n (%)                 <NA>                 <NA>
      19                  3             15 (79%)               0 (0%)
      20                  4              4 (21%)              8 (62%)
      21                  5               0 (0%)              5 (38%)
      22        carb, n (%)                 <NA>                 <NA>
      23                  1              3 (16%)              4 (31%)
      24                  2              6 (32%)              4 (31%)
      25                  3              3 (16%)               0 (0%)
      26                  4              7 (37%)              3 (23%)
      27                  6               0 (0%)             1 (7.7%)
      28                  8               0 (0%)             1 (7.7%)

---

    Code
      tbl00 %>% as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13 **p-value**
      1                 mpg                 <NA>                 <NA>       0.002
      2        Median (IQR)    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)        <NA>
      3          cyl, n (%)                 <NA>                 <NA>       0.009
      4                   4              3 (16%)              8 (62%)        <NA>
      5                   6              4 (21%)              3 (23%)        <NA>
      6                   8             12 (63%)              2 (15%)        <NA>
      7                disp                 <NA>                 <NA>      <0.001
      8        Median (IQR)       276 (196, 360)        120 (79, 160)        <NA>
      9                  hp                 <NA>                 <NA>       0.046
      10       Median (IQR)       175 (117, 193)        109 (66, 113)        <NA>
      11               drat                 <NA>                 <NA>      <0.001
      12       Median (IQR)    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)        <NA>
      13                 wt                 <NA>                 <NA>      <0.001
      14       Median (IQR)    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)        <NA>
      15               qsec                 <NA>                 <NA>         0.3
      16       Median (IQR) 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)        <NA>
      17          vs, n (%)              7 (37%)              7 (54%)         0.3
      18        gear, n (%)                 <NA>                 <NA>      <0.001
      19                  3             15 (79%)               0 (0%)        <NA>
      20                  4              4 (21%)              8 (62%)        <NA>
      21                  5               0 (0%)              5 (38%)        <NA>
      22        carb, n (%)                 <NA>                 <NA>         0.3
      23                  1              3 (16%)              4 (31%)        <NA>
      24                  2              6 (32%)              4 (31%)        <NA>
      25                  3              3 (16%)               0 (0%)        <NA>
      26                  4              7 (37%)              3 (23%)        <NA>
      27                  6               0 (0%)             1 (7.7%)        <NA>
      28                  8               0 (0%)             1 (7.7%)        <NA>

---

    Code
      tbl %>% add_overall() %>% add_stat_label() %>% as.data.frame()
    Output
         **Characteristic**  **Overall**, N = 32        **0**, N = 19
      1                 mpg                 <NA>                 <NA>
      2        Median (IQR)    19.2 (15.4, 22.8)    17.3 (15.0, 19.2)
      3          cyl, n (%)                 <NA>                 <NA>
      4                   4             11 (34%)              3 (16%)
      5                   6              7 (22%)              4 (21%)
      6                   8             14 (44%)             12 (63%)
      7                disp                 <NA>                 <NA>
      8        Median (IQR)       196 (121, 326)       276 (196, 360)
      9                  hp                 <NA>                 <NA>
      10       Median (IQR)        123 (97, 180)       175 (117, 193)
      11               drat                 <NA>                 <NA>
      12       Median (IQR)    3.70 (3.08, 3.92)    3.15 (3.07, 3.70)
      13                 wt                 <NA>                 <NA>
      14       Median (IQR)    3.33 (2.58, 3.61)    3.52 (3.44, 3.84)
      15               qsec                 <NA>                 <NA>
      16       Median (IQR) 17.71 (16.89, 18.90) 17.82 (17.18, 19.17)
      17          vs, n (%)             14 (44%)              7 (37%)
      18        gear, n (%)                 <NA>                 <NA>
      19                  3             15 (47%)             15 (79%)
      20                  4             12 (38%)              4 (21%)
      21                  5              5 (16%)               0 (0%)
      22        carb, n (%)                 <NA>                 <NA>
      23                  1              7 (22%)              3 (16%)
      24                  2             10 (31%)              6 (32%)
      25                  3             3 (9.4%)              3 (16%)
      26                  4             10 (31%)              7 (37%)
      27                  6             1 (3.1%)               0 (0%)
      28                  8             1 (3.1%)               0 (0%)
                **1**, N = 13
      1                  <NA>
      2     22.8 (21.0, 30.4)
      3                  <NA>
      4               8 (62%)
      5               3 (23%)
      6               2 (15%)
      7                  <NA>
      8         120 (79, 160)
      9                  <NA>
      10        109 (66, 113)
      11                 <NA>
      12    4.08 (3.85, 4.22)
      13                 <NA>
      14    2.32 (1.94, 2.78)
      15                 <NA>
      16 17.02 (16.46, 18.61)
      17              7 (54%)
      18                 <NA>
      19               0 (0%)
      20              8 (62%)
      21              5 (38%)
      22                 <NA>
      23              4 (31%)
      24              4 (31%)
      25               0 (0%)
      26              3 (23%)
      27             1 (7.7%)
      28             1 (7.7%)

---

    Code
      tbl %>% add_stat_label(location = "column", label = all_categorical() ~
        "no. (%)") %>% as.data.frame()
    Output
         **Characteristic** **Statistic**        **0**, N = 19        **1**, N = 13
      1                 mpg          <NA>                 <NA>                 <NA>
      2        Median (IQR)          <NA>    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)
      3                 cyl          <NA>                 <NA>                 <NA>
      4                   4       no. (%)              3 (16%)              8 (62%)
      5                   6       no. (%)              4 (21%)              3 (23%)
      6                   8       no. (%)             12 (63%)              2 (15%)
      7                disp          <NA>                 <NA>                 <NA>
      8        Median (IQR)          <NA>       276 (196, 360)        120 (79, 160)
      9                  hp          <NA>                 <NA>                 <NA>
      10       Median (IQR)          <NA>       175 (117, 193)        109 (66, 113)
      11               drat          <NA>                 <NA>                 <NA>
      12       Median (IQR)          <NA>    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)
      13                 wt          <NA>                 <NA>                 <NA>
      14       Median (IQR)          <NA>    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)
      15               qsec          <NA>                 <NA>                 <NA>
      16       Median (IQR)          <NA> 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)
      17                 vs       no. (%)              7 (37%)              7 (54%)
      18               gear          <NA>                 <NA>                 <NA>
      19                  3       no. (%)             15 (79%)               0 (0%)
      20                  4       no. (%)              4 (21%)              8 (62%)
      21                  5       no. (%)               0 (0%)              5 (38%)
      22               carb          <NA>                 <NA>                 <NA>
      23                  1       no. (%)              3 (16%)              4 (31%)
      24                  2       no. (%)              6 (32%)              4 (31%)
      25                  3       no. (%)              3 (16%)               0 (0%)
      26                  4       no. (%)              7 (37%)              3 (23%)
      27                  6       no. (%)               0 (0%)             1 (7.7%)
      28                  8       no. (%)               0 (0%)             1 (7.7%)

# no errors/warnings with standard use for tbl_svysummary

    Code
      tbl %>% add_stat_label() %>% as.data.frame()
    Output
                           **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age, Median (IQR)        46 (37, 59)         48 (39, 56)
      2                               Unknown                  7                   4
      3    Marker Level (ng/mL), Median (IQR)  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)
      4                               Unknown                  6                   4
      5                        T Stage, n (%)               <NA>                <NA>
      6                                    T1           28 (29%)            25 (25%)
      7                                    T2           25 (26%)            29 (28%)
      8                                    T3           22 (22%)            21 (21%)
      9                                    T4           23 (23%)            27 (26%)
      10                         Grade, n (%)               <NA>                <NA>
      11                                    I           35 (36%)            33 (32%)
      12                                   II           32 (33%)            36 (35%)
      13                                  III           31 (32%)            33 (32%)
      14                Tumor Response, n (%)           28 (29%)            33 (34%)
      15                              Unknown                  3                   4
      16                  Patient Died, n (%)           52 (53%)            60 (59%)
      17 Months to Death/Censor, Median (IQR)  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)

---

    Code
      tbl %>% add_stat_label(location = "column", label = all_categorical() ~
        "no. (%)") %>% as.data.frame()
    Output
             **Characteristic** **Statistic** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age  Median (IQR)        46 (37, 59)         48 (39, 56)
      2                 Unknown             n                  7                   4
      3    Marker Level (ng/mL)  Median (IQR)  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)
      4                 Unknown             n                  6                   4
      5                 T Stage          <NA>               <NA>                <NA>
      6                      T1       no. (%)           28 (29%)            25 (25%)
      7                      T2       no. (%)           25 (26%)            29 (28%)
      8                      T3       no. (%)           22 (22%)            21 (21%)
      9                      T4       no. (%)           23 (23%)            27 (26%)
      10                  Grade          <NA>               <NA>                <NA>
      11                      I       no. (%)           35 (36%)            33 (32%)
      12                     II       no. (%)           32 (33%)            36 (35%)
      13                    III       no. (%)           31 (32%)            33 (32%)
      14         Tumor Response       no. (%)           28 (29%)            33 (34%)
      15                Unknown             n                  3                   4
      16           Patient Died       no. (%)           52 (53%)            60 (59%)
      17 Months to Death/Censor  Median (IQR)  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)

# no errors/warnings with standard use for tbl_svysummary with continuous2

    Code
      tbl %>% add_stat_label() %>% as.data.frame()
    Output
             **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age               <NA>                <NA>
      2            Median (IQR)        46 (37, 59)         48 (39, 56)
      3                 Unknown                  7                   4
      4    Marker Level (ng/mL)               <NA>                <NA>
      5            Median (IQR)  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)
      6                 Unknown                  6                   4
      7          T Stage, n (%)               <NA>                <NA>
      8                      T1           28 (29%)            25 (25%)
      9                      T2           25 (26%)            29 (28%)
      10                     T3           22 (22%)            21 (21%)
      11                     T4           23 (23%)            27 (26%)
      12           Grade, n (%)               <NA>                <NA>
      13                      I           35 (36%)            33 (32%)
      14                     II           32 (33%)            36 (35%)
      15                    III           31 (32%)            33 (32%)
      16  Tumor Response, n (%)           28 (29%)            33 (34%)
      17                Unknown                  3                   4
      18    Patient Died, n (%)           52 (53%)            60 (59%)
      19 Months to Death/Censor               <NA>                <NA>
      20           Median (IQR)  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)

---

    Code
      tbl %>% add_stat_label(location = "column", label = all_categorical() ~
        "no. (%)") %>% as.data.frame()
    Output
             **Characteristic** **Statistic** **Drug A**, N = 98 **Drug B**, N = 102
      1                     Age          <NA>               <NA>                <NA>
      2            Median (IQR)          <NA>        46 (37, 59)         48 (39, 56)
      3                 Unknown             n                  7                   4
      4    Marker Level (ng/mL)          <NA>               <NA>                <NA>
      5            Median (IQR)          <NA>  0.82 (0.23, 1.55)   0.51 (0.18, 1.20)
      6                 Unknown             n                  6                   4
      7                 T Stage          <NA>               <NA>                <NA>
      8                      T1       no. (%)           28 (29%)            25 (25%)
      9                      T2       no. (%)           25 (26%)            29 (28%)
      10                     T3       no. (%)           22 (22%)            21 (21%)
      11                     T4       no. (%)           23 (23%)            27 (26%)
      12                  Grade          <NA>               <NA>                <NA>
      13                      I       no. (%)           35 (36%)            33 (32%)
      14                     II       no. (%)           32 (33%)            36 (35%)
      15                    III       no. (%)           31 (32%)            33 (32%)
      16         Tumor Response       no. (%)           28 (29%)            33 (34%)
      17                Unknown             n                  3                   4
      18           Patient Died       no. (%)           52 (53%)            60 (59%)
      19 Months to Death/Censor          <NA>               <NA>                <NA>
      20           Median (IQR)          <NA>  23.4 (17.2, 24.0)   20.9 (14.5, 24.0)

# add_stat_label() with tbl_merge()

    Code
      tbl1 %>% as.data.frame()
    Output
           **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1     Age, Median (IQR)        46 (37, 59)         48 (39, 56)
      2 Tumor Response, n (%)           28 (29%)            33 (34%)
        **Drug A**, N = 98 **Drug B**, N = 102
      1        46 (37, 59)         48 (39, 56)
      2           28 (29%)            33 (34%)

