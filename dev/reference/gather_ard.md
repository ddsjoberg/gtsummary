# Extract ARDs

Extract the ARDs from a gtsummary table. If needed, results may be
combined with
[`cards::bind_ard()`](https://pharmaverse.github.io/cards/latest-tag/reference/bind_ard.html).

## Usage

``` r
gather_ard(x)
```

## Arguments

- x:

  (`gtsummary`)  
  a gtsummary table.

## Value

list

## Examples

``` r
tbl_summary(trial, by = trt, include = age) |>
  add_overall() |>
  add_p() |>
  gather_ard()
#> $tbl_summary
#> # An ARD data frame: 27 × 12
#>    group1 group1_level variable variable_level stat_name stat                  
#>    <chr>  <list>       <chr>    <list>         <chr>     <list>                
#>  1 trt    Drug A       age      <NULL>         median    46                    
#>  2 trt    Drug A       age      <NULL>         p25       37                    
#>  3 trt    Drug A       age      <NULL>         p75       60                    
#>  4 trt    Drug B       age      <NULL>         median    48                    
#>  5 trt    Drug B       age      <NULL>         p25       39                    
#>  6 trt    Drug B       age      <NULL>         p75       56                    
#>  7 NA     <NULL>       age      <NULL>         label     Age                   
#>  8 NA     <NULL>       age      <NULL>         class     numeric               
#>  9 NA     <NULL>       trt      <NULL>         label     Chemotherapy Treatment
#> 10 NA     <NULL>       trt      <NULL>         class     character             
#> # ℹ 17 more rows
#> # ℹ 6 more variables: context <chr>, stat_label <chr>, fmt_fun <list>,
#> #   warning <list>, error <list>, gts_column <chr>
#> 
#> $add_overall
#> # An ARD data frame: 11 × 9
#>    variable        context    stat_name stat_label    stat    fmt_fun gts_column
#>    <chr>           <chr>      <chr>     <chr>         <list>  <list>  <chr>     
#>  1 age             summary    median    Median        47      <fn>    stat_0    
#>  2 age             summary    p25       Q1            38      <fn>    stat_0    
#>  3 age             summary    p75       Q3            57      <fn>    stat_0    
#>  4 age             attributes label     Variable Lab… Age     <fn>    NA        
#>  5 age             attributes class     Variable Cla… numeric <NULL>  NA        
#>  6 age             missing    N_obs     No. obs.      200     <fn>    stat_0    
#>  7 age             missing    N_miss    N Missing     11      <fn>    stat_0    
#>  8 age             missing    N_nonmiss N Non-missing 189     <fn>    stat_0    
#>  9 age             missing    p_miss    % Missing     0.055   <fn>    stat_0    
#> 10 age             missing    p_nonmiss % Non-missing 0.945   <fn>    stat_0    
#> 11 ..ard_total_n.. total_n    N         N             200     0       NA        
#> # ℹ 2 more variables: warning <list>, error <list>
#> 
#> $add_p
#> $add_p$age
#> # An ARD data frame: 15 × 9
#>    group1 variable context   stat_name stat_label stat                   fmt_fun
#>    <chr>  <chr>    <chr>     <chr>     <chr>      <list>                 <named>
#>  1 trt    age      stats_wi… estimate  Median of… -0.9999612             <fn>   
#>  2 trt    age      stats_wi… statistic X-squared… 4323                   <fn>   
#>  3 trt    age      stats_wi… p.value   p-value    0.7183173              <fn>   
#>  4 trt    age      stats_wi… conf.low  CI Lower … -4.99998               <fn>   
#>  5 trt    age      stats_wi… conf.high CI Upper … 3.999954               <fn>   
#>  6 trt    age      stats_wi… method    method     Wilcoxon rank sum test <NULL> 
#>  7 trt    age      stats_wi… alternat… alternati… two.sided              <NULL> 
#>  8 trt    age      stats_wi… mu        mu         0                      1      
#>  9 trt    age      stats_wi… paired    Paired te… FALSE                  <NULL> 
#> 10 trt    age      stats_wi… exact     exact      NA                     <NULL> 
#> 11 trt    age      stats_wi… correct   correct    TRUE                   <NULL> 
#> 12 trt    age      stats_wi… conf.int  conf.int   TRUE                   <NULL> 
#> 13 trt    age      stats_wi… conf.lev… CI Confid… 0.95                   1      
#> 14 trt    age      stats_wi… tol.root  tol.root   1e-04                  1      
#> 15 trt    age      stats_wi… digits.r… digits.ra… Inf                    1      
#> # ℹ 2 more variables: warning <named list>, error <named list>
#> 
#> 
glm(response ~ trt, data = trial, family = binomial()) |>
  tbl_regression() |>
  gather_ard()
#> $tbl_regression
#> # An ARD data frame: 29 × 9
#>    variable variable_level context   stat_name stat_label stat                  
#>    <chr>    <named list>   <chr>     <chr>     <chr>      <named list>          
#>  1 trt      Drug A         regressi… term      term       trtDrug A             
#>  2 trt      Drug A         regressi… var_label Label      Chemotherapy Treatment
#>  3 trt      Drug A         regressi… var_class Class      character             
#>  4 trt      Drug A         regressi… var_type  Type       dichotomous           
#>  5 trt      Drug A         regressi… var_nlev… N Levels   2                     
#>  6 trt      Drug A         regressi… contrasts contrasts  contr.treatment       
#>  7 trt      Drug A         regressi… contrast… Contrast … treatment             
#>  8 trt      Drug A         regressi… referenc… reference… TRUE                  
#>  9 trt      Drug A         regressi… label     Level Lab… Drug A                
#> 10 trt      Drug A         regressi… n_obs     N Obs.     95                    
#> # ℹ 19 more rows
#> # ℹ 3 more variables: fmt_fun <named list>, warning <named list>,
#> #   error <named list>
#> 
```
