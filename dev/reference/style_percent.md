# Style percentages

Style percentages

## Usage

``` r
style_percent(
  x,
  digits = 0,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  prefix = "",
  suffix = "",
  symbol,
  na = NA_character_,
  ...
)
```

## Arguments

- x:

  numeric vector of percentages

- digits:

  number of digits to round large percentages (i.e. greater than 10%).
  Smaller percentages are rounded to `digits + 1` places. Default is `0`

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

- symbol:

  Logical indicator to include percent symbol in output. Default is
  `FALSE`.

- na:

  (`NA`/`string`)  
  Character to replace `NA` values with. Default is `NA_character`

- ...:

  Arguments passed on to
  [`base::format()`](https://rdrr.io/r/base/format.html)

## Value

A character vector of styled percentages

## Author

Daniel D. Sjoberg

## Examples

``` r
percent_vals <- c(-1, 0, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)
style_percent(percent_vals)
#> [1] NA     "0"    "<0.1" "0.5"  "1.0"  "10"   "45"   "99"   "145" 
style_percent(percent_vals, suffix = "%", digits = 1)
#> [1] NA       "0%"     "0.01%"  "0.50%"  "1.00%"  "10.0%"  "45.4%"  "99.0%" 
#> [9] "145.0%"
```
