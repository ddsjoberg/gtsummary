# no errors/warnings with standard use in tbl_summary() and add_p()

    Code
      bold_p(tbl_summary_comp) %>% as.data.frame()
    Output
         **Characteristic**        **0**, N = 19        **1**, N = 13 **p-value**
      1                 mpg    17.3 (15.0, 19.2)    22.8 (21.0, 30.4)   __0.002__
      2                 cyl                 <NA>                 <NA>   __0.009__
      3                   4              3 (16%)              8 (62%)        <NA>
      4                   6              4 (21%)              3 (23%)        <NA>
      5                   8             12 (63%)              2 (15%)        <NA>
      6                disp       276 (196, 360)        120 (79, 160)  __<0.001__
      7                  hp       175 (117, 193)        109 (66, 113)   __0.046__
      8                drat    3.15 (3.07, 3.70)    4.08 (3.85, 4.22)  __<0.001__
      9                  wt    3.52 (3.44, 3.84)    2.32 (1.94, 2.78)  __<0.001__
      10               qsec 17.82 (17.18, 19.17) 17.02 (16.46, 18.61)         0.3
      11                 vs              7 (37%)              7 (54%)         0.3
      12               gear                 <NA>                 <NA>  __<0.001__
      13                  3             15 (79%)               0 (0%)        <NA>
      14                  4              4 (21%)              8 (62%)        <NA>
      15                  5               0 (0%)              5 (38%)        <NA>
      16               carb                 <NA>                 <NA>         0.3
      17                  1              3 (16%)              4 (31%)        <NA>
      18                  2              6 (32%)              4 (31%)        <NA>
      19                  3              3 (16%)               0 (0%)        <NA>
      20                  4              7 (37%)              3 (23%)        <NA>
      21                  6               0 (0%)             1 (7.7%)        <NA>
      22                  8               0 (0%)             1 (7.7%)        <NA>
