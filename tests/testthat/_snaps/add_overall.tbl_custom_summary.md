# add_overall.tbl_custom_summary() works

    Code
      as.data.frame(add_overall(tbl_custom_summary(trial, include = c("stage", "grade"), by = "trt", stat_fns = everything() ~
        my_stats, type = everything() ~ "continuous2", statistic = everything() ~ "S: {marker_sum}")))
    Output
        **Characteristic** **Overall**  \nN = 200 **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1            T Stage                   <NA>                 <NA>                  <NA>
      2      S: marker_sum                 S: 174                S: 94                 S: 80
      3              Grade                   <NA>                 <NA>                  <NA>
      4      S: marker_sum                 S: 174                S: 94                 S: 80

