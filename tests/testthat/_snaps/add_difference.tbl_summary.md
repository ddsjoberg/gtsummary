# add_difference.tbl_summary() works with basic usage

    Code
      as.data.frame(modify_column_hide(tbl_diff, all_stat_cols()))
    Output
          **Characteristic** **Difference**  **95% CI** **p-value**
      1 Marker Level (ng/mL)           0.20 -0.05, 0.44        0.12
      2                  Age          -0.44   -4.6, 3.7         0.8

---

    Code
      as.data.frame(modify_column_hide(add_difference(tbl_summary(select(trial, trt,
        response, grade), by = trt, percent = "row")), all_stat_cols()))
    Condition
      Warning:
      The `add_difference()` results for categorical variables may not compatible with `tbl_summary(percent = c('cell', 'row'))`.
      i Use column percentages instead, `tbl_summary(percent = 'column')`.
    Output
        **Characteristic** **Difference**  **95% CI** **p-value**
      1     Tumor Response          -4.2%  -18%, 9.9%         0.6
      2            Unknown           <NA>        <NA>        <NA>
      3              Grade           0.07 -0.20, 0.35        <NA>
      4                  I           <NA>        <NA>        <NA>
      5                 II           <NA>        <NA>        <NA>
      6                III           <NA>        <NA>        <NA>

# statistics are replicated within add_difference.tbl_summary()

    Code
      as.data.frame(modify_column_hide(tbl_test.args, all_stat_cols()))
    Output
           **Characteristic** **Difference**  **95% CI** **p-value**
      1            var_t.test          -0.44   -4.6, 3.7         0.8
      2       var_t.test_dots          -0.44   -4.6, 3.7         0.8
      3       var_wilcox.test           -1.0   -5.0, 4.0         0.7
      4  var_wilcox.test_dots           -1.0   -5.0, 4.0         0.7
      5         var_prop.test          -4.2%  -18%, 9.9%         0.6
      6    var_prop.test_dots          -4.2%  -16%, 100%         0.7
      7            var_ancova          -0.44   -4.6, 3.7         0.8
      8          var_cohens_d          -0.03 -0.32, 0.25        <NA>
      9          var_hedges_g          -0.03 -0.31, 0.25        <NA>
      10              var_smd          -0.03 -0.32, 0.25        <NA>

# statistics are replicated within add_difference.tbl_summary(group)

    Code
      as.data.frame(modify_column_hide(tbl_groups, all_stat_cols()))
    Output
         **Characteristic** **Difference**  **95% CI** **p-value**
      1     age_ancova_lme4          -0.57   -4.0, 2.8        <NA>
      2   age_paired_t_test          -0.85   -4.4, 2.7         0.6
      3 age_paired_cohens_d          -0.05 -0.26, 0.16        <NA>
      4 age_paired_hedges_g          -0.05 -0.26, 0.16        <NA>

# row formatting of differences and CIs work

    Code
      tbl1
    Output
      # A tibble: 4 x 6
        label                stat_1      stat_2      estimate conf.low    p.value
        <chr>                <chr>       <chr>       <chr>    <chr>       <chr>  
      1 Age                  47 (15)     47 (14)     -0.44    -4.6, 3.7   0.8    
      2 Marker Level (ng/mL) 1.02 (0.89) 0.82 (0.83) 0.20     -0.05, 0.44 0.12   
      3 Tumor Response       29%         34%         -4.2%    -18%, 9.9%  0.6    
      4 Patient Died         53%         59%         -5.8%    -21%, 9.0%  0.5    

# no error with missing data

    Code
      as.data.frame(modify_column_hide(t1, all_stat_cols()))
    Output
        **Characteristic** **Difference** **95% CI** **p-value**
      1                mpg           <NA>       <NA>        <NA>
      2                 hp           <NA>       <NA>        <NA>

# add_difference() with smd

    Code
      tbl
    Output
                 label      stat_1      stat_2 estimate    conf.low
      1            Age 46 (37, 60) 48 (39, 56)    -0.03 -0.32, 0.25
      2 Tumor Response    28 (29%)    33 (34%)    -0.09 -0.37, 0.19
      3          Grade        <NA>        <NA>     0.07 -0.20, 0.35
      4              I    35 (36%)    33 (32%)     <NA>        <NA>
      5             II    32 (33%)    36 (35%)     <NA>        <NA>
      6            III    31 (32%)    33 (32%)     <NA>        <NA>

# add_difference.tbl_summary() with emmeans()

    Code
      as.data.frame(modify_column_hide(res, all_stat_cols()))
    Output
        **Characteristic** **Adjusted Difference** **95% CI** **p-value**
      1                Age                   -0.42  -4.5, 3.7         0.8
      2     Tumor Response                   -4.7% -18%, 8.4%         0.5

# ordering in add_difference.tbl_summary() with paired tests

    Code
      as.data.frame(modify_column_hide(add_difference(mutate(mtcars, .by = am, id = dplyr::row_number(),
      am = factor(am, levels = c(0, 1))) %>% tbl_summary(by = am, include = mpg),
      test = ~"paired.t.test", group = id), all_stat_cols()))
    Message
      The following warning was returned in `add_difference()` for variable "mpg"
      ! Some observations included in the stratified summary statistics were omitted from the comparison due to unbalanced missingness within group.
    Output
        **Characteristic** **Difference** **95% CI** **p-value**
      1                mpg           -7.0  -10, -3.6      <0.001

---

    Code
      as.data.frame(modify_column_hide(add_difference(tbl_summary(mutate(mtcars, .by = am,
        id = dplyr::row_number(), am = factor(am, levels = c(1, 0))), by = am,
      include = mpg), test = ~"paired.t.test", group = id), all_stat_cols()))
    Message
      The following warning was returned in `add_difference()` for variable "mpg"
      ! Some observations included in the stratified summary statistics were omitted from the comparison due to unbalanced missingness within group.
    Output
        **Characteristic** **Difference** **95% CI** **p-value**
      1                mpg            7.0    3.6, 10      <0.001

