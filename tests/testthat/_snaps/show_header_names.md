# show_header_names() works with tbl_summary()

    Code
      show_header_names(tbl_summary(trial, include = age, by = trt, missing = "no"))
    Output
      Column Name   Header                    level*         N*          n*          p*             
      label         "**Characteristic**"                     200 <int>                              
      stat_1        "**Drug A**  \nN = 98"    Drug A <chr>   200 <int>    98 <int>   0.490 <dbl>    
      stat_2        "**Drug B**  \nN = 102"   Drug B <chr>   200 <int>   102 <int>   0.510 <dbl>    
      
    Message
      * These values may be dynamically placed into headers (and other locations).
      i Review the `modify_header()` (`?gtsummary::modify()`) help for examples.

# show_header_names() works with tbl_regression

    Code
      show_header_names(tbl_regression(mod_logistic))
    Output
      Column Name   Header                 N*          N_event*    
      label         "**Characteristic**"   183 <dbl>   58 <dbl>    
      estimate      "**log(OR)**"          183 <dbl>   58 <dbl>    
      conf.low      "**95% CI**"           183 <dbl>   58 <dbl>    
      p.value       "**p-value**"          183 <dbl>   58 <dbl>    
      
    Message
      * These values may be dynamically placed into headers (and other locations).
      i Review the `modify_header()` (`?gtsummary::modify()`) help for examples.

# show_header_names() works with tbl_uvregression

    Code
      show_header_names(tbl_uvregression(trial, x = trt, include = c(marker, age),
      show_single_row = trt, method = lm))
    Output
      Column Name   Header           
      label         "**Outcome**"    
      stat_n        "**N**"          
      estimate      "**Beta**"       
      conf.low      "**95% CI**"     
      p.value       "**p-value**"    
      
    Message
      * These values may be dynamically placed into headers (and other locations).
      i Review the `modify_header()` (`?gtsummary::modify()`) help for examples.

# show_header_names() works with tbl_survfit

    Code
      show_header_names(tbl_survfit(trial, include = trt, y = "Surv(ttdeath, death)",
        probs = 0.5))
    Output
      Column Name   Header                 prob*          
      label         "**Characteristic**"                  
      stat_1        "**50% Percentile**"   0.500 <dbl>    
      
    Message
      * These values may be dynamically placed into headers (and other locations).
      i Review the `modify_header()` (`?gtsummary::modify()`) help for examples.

# show_header_names() returns fallback value for unknown class

    Code
      show_header_names(test_table)
    Output
      Column Name   Header                    level*         N*          n*          p*             
      label         "**Characteristic**"                     200 <???>                              
      stat_1        "**Drug A**  \nN = 98"    Drug A <chr>   200 <???>    98 <int>   0.490 <dbl>    
      stat_2        "**Drug B**  \nN = 102"   Drug B <chr>   200 <???>   102 <int>   0.510 <dbl>    
      
    Message
      * These values may be dynamically placed into headers (and other locations).
      i Review the `modify_header()` (`?gtsummary::modify()`) help for examples.

# show_header_names() returns single class value

    Code
      show_header_names(test_table)
    Output
      Column Name   Header                    level*         N*          n*          p*             
      label         "**Characteristic**"                     200 <int>                              
      stat_1        "**Drug A**  \nN = 98"    Drug A <chr>   200 <int>    98 <int>   0.490 <dbl>    
      stat_2        "**Drug B**  \nN = 102"   Drug B <chr>   200 <int>   102 <int>   0.510 <dbl>    
      
    Message
      * These values may be dynamically placed into headers (and other locations).
      i Review the `modify_header()` (`?gtsummary::modify()`) help for examples.

