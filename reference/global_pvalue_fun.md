# Global p-value generic

An S3 generic that serves as the default for `add_global_p(anova_fun)`.

The default function uses
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) (via
[`cardx::ard_car_anova()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_car_anova.html))
to calculate the p-values.

The method for GEE models (created from
[`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html))
returns Wald tests calculated using
[`aod::wald.test()`](https://rdrr.io/pkg/aod/man/wald.test.html) (via
[`cardx::ard_aod_wald_test()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_aod_wald_test.html)).
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
#> {cards} data frame: 10 x 8
#>    variable   context stat_name stat_label    stat fmt_fun
#> 1     stage car_anova     sumsq      sumsq 548.715       1
#> 2     stage car_anova        df  Degrees …       3       1
#> 3     stage car_anova    meansq     meansq 182.905       1
#> 4     stage car_anova statistic  Statistic   0.885       1
#> 5     stage car_anova   p.value    p-value    0.45       1
#> 6     grade car_anova     sumsq      sumsq 166.309       1
#> 7     grade car_anova        df  Degrees …       2       1
#> 8     grade car_anova    meansq     meansq  83.154       1
#> 9     grade car_anova statistic  Statistic   0.402       1
#> 10    grade car_anova   p.value    p-value   0.669       1
#> ℹ 2 more variables: warning, error
```
