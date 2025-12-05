# Style p-values

Style p-values

## Usage

``` r
style_pvalue(
  x,
  digits = 1,
  prepend_p = FALSE,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  na = NA_character_,
  ...
)
```

## Arguments

- x:

  (`numeric`)  
  Numeric vector of p-values.

- digits:

  (`integer`)  
  Number of digits large p-values are rounded. Must be 1, 2, or 3.
  Default is 1.

- prepend_p:

  (scalar `logical`)  
  Logical. Should 'p=' be prepended to formatted p-value. Default is
  `FALSE`

- big.mark:

  (`string`)  
  Character used between every 3 digits to separate
  hundreds/thousands/millions/etc. Default is `","`, except when
  `decimal.mark = ","` when the default is a space.

- decimal.mark:

  (`string`)  
  The character to be used to indicate the numeric decimal point.
  Default is `"."` or `getOption("OutDec")`

- na:

  (`NA`/`string`)  
  Character to replace `NA` values with. Default is `NA_character`

- ...:

  Arguments passed on to
  [`base::format()`](https://rdrr.io/r/base/format.html)

## Value

A character vector of styled p-values

## Author

Daniel D. Sjoberg

## Examples

``` r
pvals <- c(
  1.5, 1, 0.999, 0.5, 0.25, 0.2, 0.197, 0.12, 0.10, 0.0999, 0.06,
  0.03, 0.002, 0.001, 0.00099, 0.0002, 0.00002, -1
)
style_pvalue(pvals)
#>  [1] NA       ">0.9"   ">0.9"   "0.5"    "0.3"    "0.2"    "0.2"    "0.12"  
#>  [9] "0.10"   "0.10"   "0.060"  "0.030"  "0.002"  "0.001"  "<0.001" "<0.001"
#> [17] "<0.001" NA      
style_pvalue(pvals, digits = 2, prepend_p = TRUE)
#>  [1] NA        "p>0.99"  "p>0.99"  "p=0.50"  "p=0.25"  "p=0.20"  "p=0.20" 
#>  [8] "p=0.12"  "p=0.10"  "p=0.10"  "p=0.060" "p=0.030" "p=0.002" "p=0.001"
#> [15] "p<0.001" "p<0.001" "p<0.001" NA       
```
