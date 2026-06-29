# Assign Default Digits

Used to assign the default formatting for variables summarized with
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).

## Usage

``` r
assign_summary_digits(data, statistic, type, digits = NULL)
```

## Arguments

- data:

  (`data.frame`)  
  a data frame

- statistic:

  (`named list`)  
  a named list; notably, *not* a
  [`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md)

- type:

  (`named list`)  
  a named list; notably, *not* a
  [`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md)

- digits:

  (`named list`)  
  a named list; notably, *not* a
  [`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md).
  Default is `NULL`

## Value

a named list

## Examples

``` r
assign_summary_digits(
  mtcars,
  statistic = list(mpg = "{mean}"),
  type = list(mpg = "continuous")
)
#> $mpg
#> $mpg$mean
#> function (x) 
#> style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, 
#>     scale = scale, prefix = prefix, suffix = suffix, na = na, 
#>     ...)
#> <bytecode: 0x565106635080>
#> <environment: 0x565110d87018>
#> 
#> $mpg$N_obs
#> function (x) 
#> style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, 
#>     scale = scale, prefix = prefix, suffix = suffix, na = na, 
#>     ...)
#> <bytecode: 0x565106635080>
#> <environment: 0x565106fb87a0>
#> 
#> $mpg$N_miss
#> function (x) 
#> style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, 
#>     scale = scale, prefix = prefix, suffix = suffix, na = na, 
#>     ...)
#> <bytecode: 0x565106635080>
#> <environment: 0x565106fb87a0>
#> 
#> $mpg$N_nonmiss
#> function (x) 
#> style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, 
#>     scale = scale, prefix = prefix, suffix = suffix, na = na, 
#>     ...)
#> <bytecode: 0x565106635080>
#> <environment: 0x565106fb87a0>
#> 
#> $mpg$p_miss
#> function (x) 
#> style_percent(x, prefix = prefix, suffix = suffix, digits = digits, 
#>     big.mark = big.mark, decimal.mark = decimal.mark, na = na, 
#>     ...)
#> <bytecode: 0x56510663ff28>
#> <environment: 0x56510c2cdf08>
#> 
#> $mpg$p_nonmiss
#> function (x) 
#> style_percent(x, prefix = prefix, suffix = suffix, digits = digits, 
#>     big.mark = big.mark, decimal.mark = decimal.mark, na = na, 
#>     ...)
#> <bytecode: 0x56510663ff28>
#> <environment: 0x56510c2cdf08>
#> 
#> 
```
