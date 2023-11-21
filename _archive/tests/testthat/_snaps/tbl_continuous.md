# tbl_continuous() works

    Code
      tbl1
    Output
      # A tibble: 9 x 3
        `**Characteristic**` `Drug A, N = 200` `Drug B, N = 200`
        <chr>                <chr>             <chr>            
      1 Grade                <NA>              <NA>             
      2 I                    46 (36, 60)       48 (42, 55)      
      3 II                   45 (31, 55)       51 (43, 57)      
      4 III                  52 (42, 60)       45 (36, 52)      
      5 T Stage              <NA>              <NA>             
      6 T1                   43 (31, 53)       47 (43, 57)      
      7 T2                   48 (42, 62)       49 (42, 53)      
      8 T3                   48 (38, 60)       53 (40, 59)      
      9 T4                   46 (36, 60)       45 (38, 54)      

---

    Code
      tbl_continuous(data = trial, variable = age, by = trt, include = c(grade, stage)) %>%
        add_overall() %>% as.data.frame()
    Output
        **Characteristic** **Overall**, N = 200 **Drug A**, N = 98
      1              Grade                 <NA>               <NA>
      2                  I          47 (37, 56)        46 (36, 60)
      3                 II          49 (37, 57)        45 (31, 55)
      4                III          47 (38, 58)        52 (42, 60)
      5            T Stage                 <NA>               <NA>
      6                 T1          45 (37, 57)        43 (31, 53)
      7                 T2          48 (42, 55)        48 (42, 62)
      8                 T3          50 (39, 60)        48 (38, 60)
      9                 T4          46 (37, 55)        46 (36, 60)
        **Drug B**, N = 102
      1                <NA>
      2         48 (42, 55)
      3         51 (43, 57)
      4         45 (36, 52)
      5                <NA>
      6         47 (43, 57)
      7         49 (42, 53)
      8         53 (40, 59)
      9         45 (38, 54)

---

    Code
      tbl2
    Output
      # A tibble: 9 x 2
        label   stat_0
        <chr>   <chr> 
      1 Grade   <NA>  
      2 I       46    
      3 II      48    
      4 III     48    
      5 T Stage <NA>  
      6 T1      47    
      7 T2      48    
      8 T3      49    
      9 T4      45    

