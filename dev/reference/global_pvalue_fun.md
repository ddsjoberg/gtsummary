# Global p-value generic

An S3 generic that serves as the default for `add_global_p(anova_fun)`.

The default function uses
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) (via
[`cardx::ard_car_anova()`](https://rdrr.io/pkg/cardx/man/ard_car_anova.html))
to calculate the p-values.

The method for GEE models (created from
[`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html))
returns Wald tests calculated using
[`aod::wald.test()`](https://rdrr.io/pkg/aod/man/wald.test.html) (via
[`cardx::ard_aod_wald_test()`](https://rdrr.io/pkg/cardx/man/ard_aod_wald_test.html)).
For this method, the `type` argument is not used.

## Usage

``` r
global_pvalue_fun(x, type, ...)

# Default S3 method
global_pvalue_fun(x, type, ...)

# S3 method for class 'geeglm'
global_pvalue_fun(x, type, ...)
```

## Value

data frame

## Examples

``` r
lm(age ~ stage + grade, trial) |>
  global_pvalue_fun(type = "III")
#> # An ARD data frame: 10 × 8
#>    variable context   stat_name stat_label           stat fmt_fun warning error 
#>    <chr>    <chr>     <chr>     <chr>              <list>  <list> <named> <name>
#>  1 stage    car_anova sumsq     sumsq             549.          1 <NULL>  <NULL>
#>  2 stage    car_anova df        Degrees of Freed…   3           1 <NULL>  <NULL>
#>  3 stage    car_anova meansq    meansq            183.          1 <NULL>  <NULL>
#>  4 stage    car_anova statistic Statistic           0.885       1 <NULL>  <NULL>
#>  5 stage    car_anova p.value   p-value             0.450       1 <NULL>  <NULL>
#>  6 grade    car_anova sumsq     sumsq             166.          1 <NULL>  <NULL>
#>  7 grade    car_anova df        Degrees of Freed…   2           1 <NULL>  <NULL>
#>  8 grade    car_anova meansq    meansq             83.2         1 <NULL>  <NULL>
#>  9 grade    car_anova statistic Statistic           0.402       1 <NULL>  <NULL>
#> 10 grade    car_anova p.value   p-value             0.669       1 <NULL>  <NULL>
```
