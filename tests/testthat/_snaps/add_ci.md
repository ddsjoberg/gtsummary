# add_ci() works

    Code
      tbl1 %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 (**95% CI**)
      1     Tumor Response             28 (29%) (21%, 40%)
      2                Age            46 (37, 59) (44, 50)
        **Drug B**, N = 102 (**95% CI**) **p-value**
      1              33 (34%) (25%, 44%)         0.5
      2             48 (39, 56) (45, 50)         0.7

---

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **95% CI** **Drug B**, N = 102
      1     Tumor Response           28 (29%)   21%, 40%            33 (34%)
      2                Age        46 (37, 59)     44, 50         48 (39, 56)
        **95% CI** **p-value**
      1   25%, 44%         0.5
      2     45, 50         0.7

---

    Code
      res %>% as.data.frame()
    Output
            **Characteristic** **N = 200** **95% CI**
      1         Tumor Response    61 (32%)   25 to 39
      2 Chemotherapy Treatment        <NA>       <NA>
      3                 Drug A    98 (49%)   42 to 56
      4                 Drug B   102 (51%)   44 to 58

# add_ci() works on a subset of variables

    Code
      trial %>% tbl_summary(include = c(response, age)) %>% add_ci(include = age) %>%
        as_tibble()
    Message
      x `add_ci()` added mean CI for "age"; however, no mean is shown in the `tbl_summary()` table.
    Output
      # A tibble: 4 x 3
        `**Characteristic**` `**N = 200**` `**95% CI**`
        <chr>                <chr>         <chr>       
      1 Tumor Response       61 (32%)      <NA>        
      2 Unknown              7             <NA>        
      3 Age                  47 (38, 57)   45, 49      
      4 Unknown              11            <NA>        

# add_ci() column order respects add_overall(last=TRUE)

    Code
      trial %>% tbl_summary(by = trt, include = "stage") %>% add_overall(last = TRUE) %>%
        add_ci() %>% as_tibble(col_label = FALSE)
    Output
      # A tibble: 5 x 7
        label   stat_1   ci_stat_1 stat_2   ci_stat_2 stat_0   ci_stat_0
        <chr>   <chr>    <chr>     <chr>    <chr>     <chr>    <chr>    
      1 T Stage <NA>     <NA>      <NA>     <NA>      <NA>     <NA>     
      2 T1      28 (29%) 20%, 39%  25 (25%) 17%, 34%  53 (27%) 21%, 33% 
      3 T2      25 (26%) 17%, 35%  29 (28%) 20%, 38%  54 (27%) 21%, 34% 
      4 T3      22 (22%) 15%, 32%  21 (21%) 13%, 30%  43 (22%) 16%, 28% 
      5 T4      23 (23%) 16%, 33%  27 (26%) 18%, 36%  50 (25%) 19%, 32% 

# add_ci() works with tbl_svysummary

    Code
      svyres %>% as_tibble()
    Output
      # A tibble: 8 x 5
        `**Characteristic**` `**No**, N = 1,692` `**95% CI**` `**Yes**, N = 4,502`
        <chr>                <chr>               <chr>        <chr>               
      1 api00                631 (556, 710)      547, 722     654 (551, 722)      
      2 hsg                  20 (12)             13, 28       22 (17)             
      3 stype                <NA>                <NA>         <NA>                
      4 E                    1,083 (64%)         43%, 81%     3,791 (84%)         
      5 H                    237 (14%)           6.6%, 27%    237 (5.3%)          
      6 M                    372 (22%)           8.7%, 46%    474 (11%)           
      7 yr.rnd               34 (2.0%)           0.32%, 12%   271 (6.0%)          
      8 logical              1,659 (98%)         88%, 100%    4,231 (94%)         
      # i 1 more variable: `**95% CI**` <chr>

---

    Code
      res %>% as_tibble()
    Output
      # A tibble: 8 x 5
        `**Characteristic**` `**No**, N = 1,692` `**95% CI**` `**Yes**, N = 4,502`
        <chr>                <chr>               <chr>        <chr>               
      1 api00                631 (556, 710)      547, 722     654 (551, 722)      
      2 hsg                  20 (12)             13, 27       22 (17)             
      3 stype                <NA>                <NA>         <NA>                
      4 E                    1,083 (64%)         46%, 82%     3,791 (84%)         
      5 H                    237 (14%)           4.8%, 23%    237 (5.3%)          
      6 M                    372 (22%)           5.0%, 39%    474 (11%)           
      7 yr.rnd               34 (2.0%)           0.37%, 10%   271 (6.0%)          
      8 logical              1,659 (98%)         90%, 100%    4,231 (94%)         
      # i 1 more variable: `**95% CI**` <chr>

