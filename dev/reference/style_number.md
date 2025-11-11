# Style numbers

Style numbers

## Usage

``` r
style_number(
  x,
  digits = 0,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  scale = 1,
  prefix = "",
  suffix = "",
  na = NA_character_,
  ...
)
```

## Arguments

- x:

  (`numeric`)  
  Numeric vector

- digits:

  (non-negative `integer`)  
  Integer or vector of integers specifying the number of decimals to
  round `x`. When vector is passed, each integer is mapped 1:1 to the
  numeric values in `x`

- big.mark:

  (`string`)  
  Character used between every 3 digits to separate
  hundreds/thousands/millions/etc. Default is `","`, except when
  `decimal.mark = ","` when the default is a space.

- decimal.mark:

  (`string`)  
  The character to be used to indicate the numeric decimal point.
  Default is `"."` or `getOption("OutDec")`

- scale:

  (scalar `numeric`)  
  A scaling factor: `x` will be multiplied by scale before formatting.

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

formatted character vector

## Examples

``` r
c(0.111, 12.3) |> style_number(digits = 1)
#> [1] "0.1"  "12.3"
c(0.111, 12.3) |> style_number(digits = c(1, 0))
#> [1] "0.1" "12" 
```
