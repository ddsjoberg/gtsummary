# Style Functions

Similar to the `style_*()` family of functions, but these functions
return a `style_*()` **function** rather than performing the styling.

## Usage

``` r
label_style_number(
  digits = 0,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  scale = 1,
  prefix = "",
  suffix = "",
  na = NA_character_,
  ...
)

label_style_sigfig(
  digits = 2,
  scale = 1,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  prefix = "",
  suffix = "",
  na = NA_character_,
  ...
)

label_style_pvalue(
  digits = 1,
  prepend_p = FALSE,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  na = NA_character_,
  ...
)

label_style_ratio(
  digits = 2,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  prefix = "",
  suffix = "",
  na = NA_character_,
  ...
)

label_style_percent(
  prefix = "",
  suffix = "",
  digits = 0,
  big.mark = ifelse(decimal.mark == ",", " ", ","),
  decimal.mark = getOption("OutDec"),
  na = NA_character_,
  ...
)
```

## Arguments

- digits, big.mark, decimal.mark, scale, prepend_p, prefix, suffix, na,
  ...:

  arguments passed to the `style_*()` functions

## Value

a function

## See also

Other style tools:
[`style_sigfig()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_sigfig.md)

## Examples

``` r
my_style <- label_style_number(digits = 1)
my_style(3.14)
#> [1] "3.1"
```
