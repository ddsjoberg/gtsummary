# tbl_summary creates output without error/warning (no by var)

    Code
      purrr::map(lst_tbl, as_tibble)
    Output
      [[1]]
      # A tibble: 23 x 2
         `**Characteristic**` `**N = 32**`        
         <chr>                <chr>               
       1 mpg                  19.2 (15.4, 22.8)   
       2 cyl                  <NA>                
       3 8                    14 (44%)            
       4 4                    11 (34%)            
       5 6                    7 (22%)             
       6 disp                 196 (121, 326)      
       7 hp                   123 (96, 180)       
       8 drat                 3.70 (3.08, 3.92)   
       9 wt                   3.33 (2.58, 3.61)   
      10 qsec                 17.71 (16.89, 18.90)
      # i 13 more rows
      
      [[2]]
      # A tibble: 8 x 2
        `**Characteristic**` `**N = 150**`    
        <chr>                <chr>            
      1 Sepal.Length         5.80 (5.10, 6.40)
      2 Sepal.Width          3.00 (2.80, 3.30)
      3 Petal.Length         4.35 (1.60, 5.10)
      4 Petal.Width          1.30 (0.30, 1.80)
      5 Species              <NA>             
      6 setosa               50 (33%)         
      7 versicolor           50 (33%)         
      8 virginica            50 (33%)         
      

# tbl_summary creates output without error/warning (with by var)

    Code
      tbl_summary(mtcars, by = am) %>% as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13
      1                 mpg    17.3 (14.9, 19.2)    22.8 (21.0, 30.4)
      2                 cyl                 <NA>                 <NA>
      3                   4              3 (16%)              8 (62%)
      4                   6              4 (21%)              3 (23%)
      5                   8             12 (63%)              2 (15%)
      6                disp       276 (196, 360)        120 (79, 160)
      7                  hp       175 (116, 192)        109 (66, 113)
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
      1             New mpg    17.3 (14.9, 19.2)    22.8 (21.0, 30.4)
      2             New cyl                 <NA>                 <NA>
      3                   4              3 (16%)              8 (62%)
      4                   6              4 (21%)              3 (23%)
      5                   8             12 (63%)              2 (15%)
      6                disp       276 (196, 360)        120 (79, 160)
      7                  hp       175 (116, 192)        109 (66, 113)
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
      9                      T1          53 (26%)
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
       4 Age                    47 (37, 56)       48 (37, 57)       47 (38, 58)      
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
       4 Age                    45 (36, 56)       48 (42, 55)       50 (39, 60)      
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
      dplyr::storms %>% dplyr::mutate(date = ISOdate(year, month, day), date_diff = difftime(
        dplyr::lag(date, 5), date, units = "days")) %>% tbl_summary() %>%
        as.data.frame()
    Output
                    **Characteristic**                             **N = 19,066**
      1                           name                                       <NA>
      2                       AL011993                                 11 (<0.1%)
      3                       AL012000                                  4 (<0.1%)
      4                       AL021992                                  5 (<0.1%)
      5                       AL021994                                  6 (<0.1%)
      6                       AL021999                                  4 (<0.1%)
      7                       AL022000                                 12 (<0.1%)
      8                       AL022001                                  5 (<0.1%)
      9                       AL022003                                  4 (<0.1%)
      10                      AL022006                                 13 (<0.1%)
      11                      AL031987                                  32 (0.2%)
      12                      AL031992                                  8 (<0.1%)
      13                      AL041991                                  7 (<0.1%)
      14                      AL042000                                 13 (<0.1%)
      15                      AL051994                                  9 (<0.1%)
      16                      AL061988                                 13 (<0.1%)
      17                      AL061995                                  9 (<0.1%)
      18                      AL061997                                  8 (<0.1%)
      19                      AL062003                                  8 (<0.1%)
      20                      AL071999                                  8 (<0.1%)
      21                      AL072002                                  5 (<0.1%)
      22                      AL072003                                  7 (<0.1%)
      23                      AL081992                                  25 (0.1%)
      24                      AL081994                                 10 (<0.1%)
      25                      AL091994                                  7 (<0.1%)
      26                      AL092000                                  6 (<0.1%)
      27                      AL092001                                  4 (<0.1%)
      28                      AL092003                                  4 (<0.1%)
      29                      AL101991                                  5 (<0.1%)
      30                      AL101993                                  5 (<0.1%)
      31                      AL101994                                  7 (<0.1%)
      32                      AL102004                                 13 (<0.1%)
      33                      AL111999                                 10 (<0.1%)
      34                      AL121991                                  22 (0.1%)
      35                      AL121999                                 12 (<0.1%)
      36                      AL141995                                 17 (<0.1%)
      37                      AL142002                                 11 (<0.1%)
      38                      AL142003                                 10 (<0.1%)
      39                      Al202011                                 14 (<0.1%)
      40                       Alberto                                 233 (1.2%)
      41                          Alex                                 125 (0.7%)
      42                        Alicia                                  25 (0.1%)
      43                         Allen                                  44 (0.2%)
      44                       Allison                                 121 (0.6%)
      45                         Alpha                                 11 (<0.1%)
      46                        Amelia                                  6 (<0.1%)
      47                           Amy                                  31 (0.2%)
      48                           Ana                                 189 (1.0%)
      49                        Andrea                                 14 (<0.1%)
      50                        Andrew                                  68 (0.4%)
      51                         Anita                                  20 (0.1%)
      52                        Arlene                                 199 (1.0%)
      53                        Arthur                                 172 (0.9%)
      54                          Babe                                 10 (<0.1%)
      55                         Barry                                 165 (0.9%)
      56                         Belle                                 18 (<0.1%)
      57                        Bertha                                 261 (1.4%)
      58                         Beryl                                 181 (0.9%)
      59                          Bess                                 13 (<0.1%)
      60                          Beta                                  52 (0.3%)
      61                          Bill                                 109 (0.6%)
      62                       Blanche                                  20 (0.1%)
      63                           Bob                                 105 (0.6%)
      64                        Bonnie                                 273 (1.4%)
      65                          Bret                                 138 (0.7%)
      66                       Candice                                 11 (<0.1%)
      67                      Caroline                                  33 (0.2%)
      68                         Cesar                                  61 (0.3%)
      69                       Chantal                                 171 (0.9%)
      70                       Charley                                 163 (0.9%)
      71                         Chris                                 198 (1.0%)
      72                         Cindy                                 164 (0.9%)
      73                         Clara                                  27 (0.1%)
      74                     Claudette                                 237 (1.2%)
      75                         Colin                                  40 (0.2%)
      76                          Cora                                 19 (<0.1%)
      77                     Cristobal                                 115 (0.6%)
      78                      Danielle                                 214 (1.1%)
      79                         Danny                                 197 (1.0%)
      80                         David                                  55 (0.3%)
      81                          Dean                                 148 (0.8%)
      82                         Debby                                 170 (0.9%)
      83                         Debra                                  7 (<0.1%)
      84                         Delta                                  73 (0.4%)
      85                        Dennis                                 244 (1.3%)
      86                         Diana                                  58 (0.3%)
      87                         Dolly                                 107 (0.6%)
      88                           Don                                  21 (0.1%)
      89                        Dorian                                 121 (0.6%)
      90                         Doris                                  29 (0.2%)
      91                       Dorothy                                 15 (<0.1%)
      92                        Dottie                                 10 (<0.1%)
      93                          Earl                                 208 (1.1%)
      94                       Edouard                                 228 (1.2%)
      95                         Eight                                  32 (0.2%)
      96                         Elena                                  46 (0.2%)
      97                        Eleven                                  5 (<0.1%)
      98                          Ella                                  27 (0.1%)
      99                        Eloise                                  46 (0.2%)
      100                         Elsa                                  43 (0.2%)
      101                        Emily                                 236 (1.2%)
      102                         Emmy                                  31 (0.2%)
      103                      Epsilon                                  84 (0.4%)
      104                        Erika                                 124 (0.7%)
      105                         Erin                                 184 (1.0%)
      106                      Ernesto                                 152 (0.8%)
      107                          Eta                                  58 (0.3%)
      108                       Evelyn                                 10 (<0.1%)
      109                       Fabian                                  97 (0.5%)
      110                          Fay                                 124 (0.7%)
      111                         Faye                                 19 (<0.1%)
      112                        Felix                                 206 (1.1%)
      113                      Fernand                                 16 (<0.1%)
      114                      Fifteen                                  39 (0.2%)
      115                        Fiona                                  55 (0.3%)
      116                         Five                                  32 (0.2%)
      117                     Florence                                 239 (1.3%)
      118                      Flossie                                 19 (<0.1%)
      119                        Floyd                                 131 (0.7%)
      120                         Four                                  9 (<0.1%)
      121                         Fran                                 104 (0.5%)
      122                      Frances                                 215 (1.1%)
      123                     Franklin                                  71 (0.4%)
      124                         Fred                                 124 (0.7%)
      125                     Frederic                                  68 (0.4%)
      126                       Frieda                                 10 (<0.1%)
      127                    Gabrielle                                 196 (1.0%)
      128                        Gamma                                  54 (0.3%)
      129                       Gaston                                 111 (0.6%)
      130                      Georges                                 103 (0.5%)
      131                         Gert                                 171 (0.9%)
      132                      Gilbert                                  49 (0.3%)
      133                       Gladys                                  46 (0.2%)
      134                       Gloria                                 144 (0.8%)
      135                      Gonzalo                                  62 (0.3%)
      136                       Gordon                                 196 (1.0%)
      137                        Grace                                 135 (0.7%)
      138                        Greta                                 12 (<0.1%)
      139                       Gustav                                 161 (0.8%)
      140                       Hallie                                 14 (<0.1%)
      141                        Hanna                                 121 (0.6%)
      142                       Harvey                                 198 (1.0%)
      143                       Helene                                 228 (1.2%)
      144                        Henri                                 154 (0.8%)
      145                      Hermine                                 124 (0.7%)
      146                        Holly                                  7 (<0.1%)
      147                         Hope                                 16 (<0.1%)
      148                     Hortense                                 119 (0.6%)
      149                         Hugo                                  64 (0.3%)
      150                     Humberto                                 158 (0.8%)
      151                          Ian                                  21 (0.1%)
      152                          Ida                                 122 (0.6%)
      153                         Igor                                  61 (0.3%)
      154                          Ike                                  62 (0.3%)
      155                       Imelda                                  9 (<0.1%)
      156                       Ingrid                                  48 (0.3%)
      157                         Iota                                  26 (0.1%)
      158                        Irene                                 176 (0.9%)
      159                         Iris                                 109 (0.6%)
      160                         Irma                                  72 (0.4%)
      161                        Isaac                                 170 (0.9%)
      162                       Isabel                                  94 (0.5%)
      163                       Isaias                                  36 (0.2%)
      164                      Isidore                                 168 (0.9%)
      165                         Ivan                                 175 (0.9%)
      166                       Jeanne                                 158 (0.8%)
      167                        Jerry                                 138 (0.7%)
      168                         Joan                                  53 (0.3%)
      169                      Joaquin                                  76 (0.4%)
      170                         Jose                                 152 (0.8%)
      171                    Josephine                                 231 (1.2%)
      172                        Joyce                                  79 (0.4%)
      173                         Juan                                  51 (0.3%)
      174                        Julia                                  84 (0.4%)
      175                       Julian                                 13 (<0.1%)
      176                       Juliet                                 16 (<0.1%)
      177                        Karen                                 132 (0.7%)
      178                         Karl                                 163 (0.9%)
      179                         Kate                                 133 (0.7%)
      180                        Katia                                  82 (0.4%)
      181                      Katrina                                  70 (0.4%)
      182                        Keith                                  74 (0.4%)
      183                       Kendra                                 11 (<0.1%)
      184                         Kirk                                  50 (0.3%)
      185                        Klaus                                  58 (0.3%)
      186                         Kyle                                 127 (0.7%)
      187                        Larry                                  88 (0.5%)
      188                        Laura                                  76 (0.4%)
      189                          Lee                                 111 (0.6%)
      190                        Lenny                                  39 (0.2%)
      191                       Leslie                                 175 (0.9%)
      192                         Lili                                 204 (1.1%)
      193                         Lisa                                 144 (0.8%)
      194                      Lorenzo                                  99 (0.5%)
      195                         Luis                                  61 (0.3%)
      196                        Marco                                 100 (0.5%)
      197                        Maria                                 161 (0.8%)
      198                      Marilyn                                  79 (0.4%)
      199                      Matthew                                  79 (0.4%)
      200                      Melissa                                  82 (0.4%)
      201                      Michael                                 102 (0.5%)
      202                     Michelle                                  35 (0.2%)
      203                        Mindy                                  24 (0.1%)
      204                        Mitch                                  78 (0.4%)
      205                       Nadine                                 127 (0.7%)
      206                         Nana                                  50 (0.3%)
      207                         Nate                                  84 (0.4%)
      208                       Nestor                                 15 (<0.1%)
      209                     Nicholas                                 101 (0.5%)
      210                       Nicole                                 110 (0.6%)
      211                         Nine                                 19 (<0.1%)
      212                     Nineteen                                  9 (<0.1%)
      213                         Noel                                 113 (0.6%)
      214                       Odette                                  66 (0.3%)
      215                         Olga                                  82 (0.4%)
      216                         Omar                                  59 (0.3%)
      217                          One                                 15 (<0.1%)
      218                         Opal                                  39 (0.2%)
      219                      Ophelia                                 175 (0.9%)
      220                        Oscar                                  49 (0.3%)
      221                         Otto                                 124 (0.7%)
      222                        Pablo                                  38 (0.2%)
      223                       Paloma                                  36 (0.2%)
      224                        Patty                                 11 (<0.1%)
      225                        Paula                                  21 (0.1%)
      226                     Paulette                                  88 (0.5%)
      227                        Peter                                  31 (0.2%)
      228                     Philippe                                 100 (0.5%)
      229                       Rafael                                  56 (0.3%)
      230                         Rene                                  32 (0.2%)
      231                      Richard                                  29 (0.2%)
      232                         Rina                                  52 (0.3%)
      233                         Rita                                  36 (0.2%)
      234                         Rose                                  22 (0.1%)
      235                      Roxanne                                  56 (0.3%)
      236                        Sally                                  28 (0.1%)
      237                          Sam                                  59 (0.3%)
      238                        Sandy                                  45 (0.2%)
      239                         Sean                                  28 (0.1%)
      240                    Sebastien                                  53 (0.3%)
      241                        Shary                                  9 (<0.1%)
      242                      Sixteen                                  8 (<0.1%)
      243                         Stan                                 17 (<0.1%)
      244                        Tammy                                  9 (<0.1%)
      245                        Tanya                                  29 (0.2%)
      246                        Teddy                                  49 (0.3%)
      247                          Ten                                  48 (0.3%)
      248                        Theta                                  33 (0.2%)
      249                        Three                                  5 (<0.1%)
      250                        Tomas                                  54 (0.3%)
      251                         Tony                                  20 (0.1%)
      252                          Two                                  30 (0.2%)
      253                        Vicky                                  24 (0.1%)
      254                       Victor                                  22 (0.1%)
      255                        Vince                                 15 (<0.1%)
      256                        Wanda                                  54 (0.3%)
      257                      Wilfred                                 17 (<0.1%)
      258                        Wilma                                  48 (0.3%)
      259                         Zeta                                  61 (0.3%)
      260                         year                       2,004 (1,993, 2,012)
      261                        month                          9.00 (8.00, 9.00)
      262                          day                                 16 (8, 24)
      263                         hour                           12.0 (5.0, 18.0)
      264                          lat                                27 (18, 34)
      265                         long                             -62 (-79, -46)
      266                       status                                       <NA>
      267                  disturbance                                 146 (0.8%)
      268                extratropical                                2,068 (11%)
      269                    hurricane                                4,684 (25%)
      270                    other low                               1,405 (7.4%)
      271       subtropical depression                                 151 (0.8%)
      272            subtropical storm                                 292 (1.5%)
      273          tropical depression                                3,525 (18%)
      274               tropical storm                                6,684 (35%)
      275                tropical wave                                 111 (0.6%)
      276                     category                                       <NA>
      277                            1                                2,478 (53%)
      278                            2                                  973 (21%)
      279                            3                                  579 (12%)
      280                            4                                  539 (12%)
      281                            5                                 115 (2.5%)
      282                      Unknown                                     14,382
      283                         wind                                45 (30, 65)
      284                     pressure                         1,000 (987, 1,007)
      285 tropicalstorm_force_diameter                               110 (0, 220)
      286                      Unknown                                      9,512
      287     hurricane_force_diameter                                   0 (0, 0)
      288                      Unknown                                      9,512
      289                         date 1975-06-27 12:00:00 to 2021-11-08 12:00:00
      290                    date_diff                          -1.0 (-2.0, -1.0)
      291                      Unknown                                          5

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
      4                     Age       47 (37, 56)       48 (37, 57)       47 (38, 58)
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
      10 Median (IQR)         123 (96, 180)    
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
      2        Median (IQR)    17.3 (14.9, 19.2)    22.8 (21.0, 30.4)
      3                 cyl                 <NA>                 <NA>
      4                   4              3 (16%)              8 (62%)
      5                   6              4 (21%)              3 (23%)
      6                   8             12 (63%)              2 (15%)
      7                disp                 <NA>                 <NA>
      8        Median (IQR)       276 (196, 360)        120 (79, 160)
      9                  hp                 <NA>                 <NA>
      10       Median (IQR)       175 (116, 192)        109 (66, 113)
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
      1         swallowing 76 (40, 100)
      2            Unknown            1
      3         salivation 81 (58, 100)
      4            Unknown            1

