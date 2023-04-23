# add_difference-basic use

    Code
      tbl_diff %>% as.data.frame()
    Output
          **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **Difference**
      1 Marker Level (ng/mL)  0.84 (0.24, 1.57)   0.52 (0.19, 1.20)           0.20
      2                  Age        46 (37, 59)         48 (39, 56)          -0.44
         **95% CI** **p-value**
      1 -0.05, 0.44        0.12
      2   -4.6, 3.7         0.8

---

    Code
      trial %>% select(trt, response, grade) %>% tbl_summary(by = trt, percent = "row") %>%
        add_difference() %>% as.data.frame()
    Message
      i `add_difference()` results for categorical variables may not compatible
      with `tbl_summary(percent = c("cell", "row"))` options. Use column
      percentages, `tbl_summary(percent = "column")`.
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **Difference**
      1     Tumor Response           28 (46%)            33 (54%)          -8.2%
      2            Unknown                  3                   4           <NA>
      3              Grade               <NA>                <NA>           0.07
      4                  I           35 (51%)            33 (49%)           <NA>
      5                 II           32 (47%)            36 (53%)           <NA>
      6                III           31 (48%)            33 (52%)           <NA>
         **95% CI** **p-value**
      1   -28%, 11%         0.5
      2        <NA>        <NA>
      3 -0.20, 0.35        <NA>
      4        <NA>        <NA>
      5        <NA>        <NA>
      6        <NA>        <NA>

# p-values are replicated within tbl_summary()

    Code
      tbl_test.args %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102 **Difference**
      1                Age        46 (37, 59)         48 (39, 56)          -0.44
      2                Age        46 (37, 59)         48 (39, 56)          -0.44
      3                Age        46 (37, 59)         48 (39, 56)           <NA>
      4                Age        46 (37, 59)         48 (39, 56)           <NA>
      5     Tumor Response           28 (29%)            33 (34%)          -4.2%
      6     Tumor Response           28 (29%)            33 (34%)          -4.2%
      7                Age        46 (37, 59)         48 (39, 56)          -0.44
      8                Age        46 (37, 59)         48 (39, 56)          -0.03
         **95% CI** **p-value**
      1   -4.6, 3.7         0.8
      2   -4.6, 3.7         0.8
      3        <NA>         0.7
      4        <NA>         0.7
      5  -18%, 9.9%         0.6
      6  -16%, 100%         0.7
      7   -4.6, 3.7         0.8
      8 -0.32, 0.25        <NA>

---

    Code
      tbl_groups %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
        **Adjusted Difference** **95% CI**
      1                    -1.1  -4.7, 2.4

# row formatting of differences and CIs work

    Code
      tbl1
    Output
      # A tibble: 4 x 6
        label                stat_1      stat_2      estimate ci          p.value
        <chr>                <chr>       <chr>       <chr>    <chr>       <chr>  
      1 Age                  47 (15)     47 (14)     -0.44    -4.6, 3.7   0.8    
      2 Marker Level (ng/mL) 1.02 (0.89) 0.82 (0.83) 0.20     -0.05, 0.44 0.12   
      3 Tumor Response       29%         34%         -4.2%    -18%, 9.9%  0.6    
      4 Patient Died         53%         59%         -5.8%    -21%, 9.0%  0.5    

# no error with missing data

    Code
      t1 %>% as.data.frame()
    Output
        **Characteristic** **0**, N = 19 **1**, N = 13 **p-value**
      1                mpg       0 (NA%)       0 (NA%)        <NA>
      2                 hp   NA (NA, NA)   NA (NA, NA)        <NA>

# add_difference() with smd

    Code
      tbl
    Output
      # A tibble: 6 x 5
        label          stat_1      stat_2      estimate ci         
        <chr>          <chr>       <chr>       <chr>    <chr>      
      1 Age            46 (37, 59) 48 (39, 56) -0.03    -0.32, 0.25
      2 Tumor Response 28 (29%)    33 (34%)    -0.09    -0.37, 0.19
      3 Grade          <NA>        <NA>        0.07     -0.20, 0.35
      4 I              35 (36%)    33 (32%)    <NA>     <NA>       
      5 II             32 (33%)    36 (35%)    <NA>     <NA>       
      6 III            31 (32%)    33 (32%)    <NA>     <NA>       

# add_difference() with smd and survey weights

    Code
      tbl
    Output
      # A tibble: 8 x 5
        label  stat_1      stat_2      estimate ci           
        <chr>  <chr>       <chr>       <chr>    <chr>        
      1 age    61 (17)     61 (16)     0.003    -0.068, 0.075
      2 sex    <NA>        <NA>        0.003    -0.068, 0.074
      3 Female 647 (42%)   648 (43%)   <NA>     <NA>         
      4 Male   876 (58%)   872 (57%)   <NA>     <NA>         
      5 race   <NA>        <NA>        0.009    -0.062, 0.080
      6 black  238 (16%)   236 (16%)   <NA>     <NA>         
      7 other  95 (6.2%)   98 (6.4%)   <NA>     <NA>         
      8 white  1,190 (78%) 1,187 (78%) <NA>     <NA>         

# add_difference() with emmeans()

    Code
      res %>% as.data.frame()
    Output
        **Characteristic** **Drug A**, N = 98 **Drug B**, N = 102
      1                Age        46 (37, 59)         48 (39, 56)
      2     Tumor Response           28 (29%)            33 (34%)
        **Adjusted Difference** **95% CI** **p-value**
      1                   -0.42  -4.5, 3.7         0.8
      2                   -4.7% -18%, 8.4%         0.5

