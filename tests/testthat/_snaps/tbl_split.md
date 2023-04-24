# no errors/warnings with standard use

    Code
      tbl_split(t1, variables = age) %>% purrr::map(as_tibble)
    Output
      [[1]]
      # A tibble: 5 x 2
        `**Characteristic**`   `**N = 200**`
        <chr>                  <chr>        
      1 Chemotherapy Treatment <NA>         
      2 Drug A                 98 (49%)     
      3 Drug B                 102 (51%)    
      4 Age                    47 (38, 57)  
      5 Unknown                11           
      
      [[2]]
      # A tibble: 15 x 2
         `**Characteristic**`   `**N = 200**`    
         <chr>                  <chr>            
       1 Marker Level (ng/mL)   0.64 (0.22, 1.39)
       2 Unknown                10               
       3 T Stage                <NA>             
       4 T1                     53 (27%)         
       5 T2                     54 (27%)         
       6 T3                     43 (22%)         
       7 T4                     50 (25%)         
       8 Grade                  <NA>             
       9 I                      68 (34%)         
      10 II                     68 (34%)         
      11 III                    64 (32%)         
      12 Tumor Response         61 (32%)         
      13 Unknown                7                
      14 Patient Died           112 (56%)        
      15 Months to Death/Censor 22.4 (16.0, 24.0)
      

