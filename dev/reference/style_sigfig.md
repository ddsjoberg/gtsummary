# Style significant figure-like rounding

Converts a numeric argument into a string that has been rounded to a
significant figure-like number. Scientific notation output is avoided,
however, and additional significant figures may be displayed for large
numbers. For example, if the number of significant digits requested is
2, 123 will be displayed (rather than 120 or 1.2x10^2).

## Usage

``` r
style_sigfig(
  x,
  digits = 2,
  scale = 1,
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

  Numeric vector

- digits:

  Integer specifying the minimum number of significant digits to display

- scale:

  (scalar `numeric`)  
  A scaling factor: `x` will be multiplied by scale before formatting.

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

A character vector of styled numbers

## Details

- Scientific notation output is avoided.

- If 2 significant figures are requested, the number is rounded to no
  more than 2 decimal places. For example, a number will be rounded to 2
  decimals places when `abs(x) < 1`, 1 decimal place when
  `abs(x) >= 1 & abs(x) < 10`, and to the nearest integer when
  `abs(x) >= 10`.

- Additional significant figures may be displayed for large numbers. For
  example, if the number of significant digits requested is 2, 123 will
  be displayed (rather than 120 or 1.2x10^2).

## See also

Other style tools:
[`label_style`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)

## Author

Daniel D. Sjoberg

## Examples

``` r
c(0.123, 0.9, 1.1234, 12.345, -0.123, -0.9, -1.1234, -132.345, NA, -0.001) %>%
  style_sigfig()
#>  [1] "0.12"  "0.90"  "1.1"   "12"    "-0.12" "-0.90" "-1.1"  "-132"  NA     
#> [10] "0.00" 
```
