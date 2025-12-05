# Assign Default Digits

Used to assign the default formatting for variables summarized with
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.md).

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
  [`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md)

- type:

  (`named list`)  
  a named list; notably, *not* a
  [`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md)

- digits:

  (`named list`)  
  a named list; notably, *not* a
  [`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md).
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
#> <bytecode: 0x56166c8838e8>
#> <environment: 0x5616704cadb8>
#> 
#> $mpg$N_obs
#> function (x) 
#> style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, 
#>     scale = scale, prefix = prefix, suffix = suffix, na = na, 
#>     ...)
#> <bytecode: 0x56166c8838e8>
#> <environment: 0x5616732c8390>
#> 
#> $mpg$N_miss
#> function (x) 
#> style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, 
#>     scale = scale, prefix = prefix, suffix = suffix, na = na, 
#>     ...)
#> <bytecode: 0x56166c8838e8>
#> <environment: 0x5616732c8390>
#> 
#> $mpg$N_nonmiss
#> function (x) 
#> style_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, 
#>     scale = scale, prefix = prefix, suffix = suffix, na = na, 
#>     ...)
#> <bytecode: 0x56166c8838e8>
#> <environment: 0x5616732c8390>
#> 
#> $mpg$p_miss
#> function (x) 
#> style_percent(x, prefix = prefix, suffix = suffix, digits = digits, 
#>     big.mark = big.mark, decimal.mark = decimal.mark, na = na, 
#>     ...)
#> <bytecode: 0x56166c892d78>
#> <environment: 0x561674087b60>
#> 
#> $mpg$p_nonmiss
#> function (x) 
#> style_percent(x, prefix = prefix, suffix = suffix, digits = digits, 
#>     big.mark = big.mark, decimal.mark = decimal.mark, na = na, 
#>     ...)
#> <bytecode: 0x56166c892d78>
#> <environment: 0x561674087b60>
#> 
#> 
```
