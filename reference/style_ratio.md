# Style ratios

When reporting ratios, such as relative risk or an odds ratio, we'll
often want the rounding to be similar on each side of the number 1. For
example, if we report an odds ratio of 0.95 with a confidence interval
of 0.70 to 1.24, we would want to round to two decimal places for all
values. In other words, 2 significant figures for numbers less than 1
and 3 significant figures 1 and larger. `style_ratio()` performs
significant figure-like rounding in this manner.

## Usage

``` r
style_ratio(
  x,
  digits = 2,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  prefix = "",
  suffix = "",
  na = NA_character_,
  ...
)
```

## Arguments

- x:

  (`numeric`) Numeric vector

- digits:

  (`integer`)  
  Integer specifying the number of significant digits to display for
  numbers below 1. Numbers larger than 1 will be be `digits + 1`.
  Default is `digits = 2`.

- big.mark:

  (`string`)  
  Character used between every 3 digits to separate
  hundreds/thousands/millions/etc. Default is `","`, except when
  `decimal.mark = ","` when the default is a space.

- decimal.mark:

  (`string`)  
  The character to be used to indicate the numeric decimal point.
  Default is `"."` or `getOption("OutDec")`

- prefix:

  (`string`)  
  Additional text to display before the number.

- suffix:

  (`string`)  
  Additional text to display after the number.

- na:

  (`NA`/`string`)  
  Character to replace `NA` values with. Default is `NA_character`

- ...:

  Arguments passed on to
  [`base::format()`](https://rdrr.io/r/base/format.html)

## Value

A character vector of styled ratios

## Author

Daniel D. Sjoberg

## Examples

``` r
c(0.123, 0.9, 1.1234, 12.345, 101.234, -0.123, -0.9, -1.1234, -12.345, -101.234) |>
  style_ratio()
#>  [1] "0.12"  "0.90"  "1.12"  "12.3"  "101"   "-0.12" "-0.90" "-1.12" "-12.3"
#> [10] "-101" 
```
