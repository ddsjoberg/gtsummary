# Extract ARDs

Extract the ARDs from a gtsummary table. If needed, results may be
combined with
[`cards::bind_ard()`](https://insightsengineering.github.io/cards/latest-tag/reference/bind_ard.html).

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
#> {cards} data frame: 27 x 12
#>    group1 group1_level variable variable_level stat_name stat_label      stat
#> 1     trt       Drug A      age                   median     Median        46
#> 2     trt       Drug A      age                      p25         Q1        37
#> 3     trt       Drug A      age                      p75         Q3        60
#> 4     trt       Drug B      age                   median     Median        48
#> 5     trt       Drug B      age                      p25         Q1        39
#> 6     trt       Drug B      age                      p75         Q3        56
#> 7    <NA>                   age                    label  Variable…       Age
#> 8    <NA>                   age                    class  Variable…   numeric
#> 9    <NA>                   trt                    label  Variable… Chemothe…
#> 10   <NA>                   trt                    class  Variable… character
#> ℹ 17 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 5 more variables: context, fmt_fun, warning, error, gts_column
#> 
#> $add_overall
#> {cards} data frame: 11 x 9
#>           variable   context stat_name stat_label    stat fmt_fun
#> 1              age   summary    median     Median      47    <fn>
#> 2              age   summary       p25         Q1      38    <fn>
#> 3              age   summary       p75         Q3      57    <fn>
#> 4              age attribut…     label  Variable…     Age    <fn>
#> 5              age attribut…     class  Variable… numeric    NULL
#> 6              age   missing     N_obs   No. obs.     200    <fn>
#> 7              age   missing    N_miss  N Missing      11    <fn>
#> 8              age   missing N_nonmiss  N Non-mi…     189    <fn>
#> 9              age   missing    p_miss  % Missing   0.055    <fn>
#> 10             age   missing p_nonmiss  % Non-mi…   0.945    <fn>
#> 11 ..ard_total_n..   total_n         N          N     200       0
#> ℹ 3 more variables: warning, error, gts_column
#> 
#> $add_p
#> $add_p$age
#> {cards} data frame: 15 x 9
#>    group1 variable   context   stat_name stat_label      stat
#> 1     trt      age stats_wi…    estimate  Median o…        -1
#> 2     trt      age stats_wi…   statistic  X-square…      4323
#> 3     trt      age stats_wi…     p.value    p-value     0.718
#> 4     trt      age stats_wi…    conf.low  CI Lower…        -5
#> 5     trt      age stats_wi…   conf.high  CI Upper…         4
#> 6     trt      age stats_wi…      method     method Wilcoxon…
#> 7     trt      age stats_wi… alternative  alternat… two.sided
#> 8     trt      age stats_wi…          mu         mu         0
#> 9     trt      age stats_wi…      paired  Paired t…     FALSE
#> 10    trt      age stats_wi…       exact      exact        NA
#> 11    trt      age stats_wi…     correct    correct      TRUE
#> 12    trt      age stats_wi…    conf.int   conf.int      TRUE
#> 13    trt      age stats_wi…  conf.level  CI Confi…      0.95
#> 14    trt      age stats_wi…    tol.root   tol.root         0
#> 15    trt      age stats_wi… digits.rank  digits.r…       Inf
#> ℹ 3 more variables: fmt_fun, warning, error
#> 
#> 

glm(response ~ trt, data = trial, family = binomial()) |>
  tbl_regression() |>
  gather_ard()
#> $tbl_regression
#> {cards} data frame: 29 x 9
#>    variable variable_level   context      stat_name stat_label      stat
#> 1       trt         Drug A regressi…           term       term trtDrug A
#> 2       trt         Drug A regressi…      var_label      Label Chemothe…
#> 3       trt         Drug A regressi…      var_class      Class character
#> 4       trt         Drug A regressi…       var_type       Type dichotom…
#> 5       trt         Drug A regressi…    var_nlevels   N Levels         2
#> 6       trt         Drug A regressi…      contrasts  contrasts contr.tr…
#> 7       trt         Drug A regressi… contrasts_type  Contrast… treatment
#> 8       trt         Drug A regressi…  reference_row  referenc…      TRUE
#> 9       trt         Drug A regressi…          label  Level La…    Drug A
#> 10      trt         Drug A regressi…          n_obs     N Obs.        95
#> ℹ 19 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 3 more variables: fmt_fun, warning, error
#> 
```
