# pkgwide-str:language works

    Code
      show_header_names(gts_2)
    Output
      Column Name   Header                    level*         N*          n*          p*             
      label         "**Característica**"                    200 <int>                              
      stat_1        "**Drug A**  \nN = 98"    Drug A <chr>   200 <int>    98 <int>   0.490 <dbl>    
      stat_2        "**Drug B**  \nN = 102"   Drug B <chr>   200 <int>   102 <int>   0.510 <dbl>    
      p.value       "**p-valor**"                            200 <int>                              
      
    Message
      * These values may be dynamically placed into headers (and other locations).
      i Review the `modify_header()` (`?gtsummary::modify()`) help for examples.

# pkgwide-str:theme_name works

    Code
      set_gtsummary_theme(my_theme_5)
    Message
      Setting theme "Super Cool Themey Theme"

# pkgwide-fun:pre_conversion works

    Code
      gts_6

