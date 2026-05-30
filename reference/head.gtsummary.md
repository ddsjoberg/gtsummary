# Display the first or last rows

These functions allow you to view the first
([`head()`](https://rdrr.io/r/utils/head.html)) or last
([`tail()`](https://rdrr.io/r/utils/head.html)) `n` rows of a gtsummary
table.

## Usage

``` r
# S3 method for class 'gtsummary'
head(x, n = 6L, ...)

# S3 method for class 'gtsummary'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  A `gtsummary` object.

- n:

  Number of rows to return. Default is `6L` as default in
  [`utils::head()`](https://rdrr.io/r/utils/head.html) and
  [`utils::tail()`](https://rdrr.io/r/utils/head.html).

- ...:

  Additional arguments passed to
  [`utils::head()`](https://rdrr.io/r/utils/head.html) or
  [`utils::tail()`](https://rdrr.io/r/utils/head.html).

## Value

A `gtsummary` object with only the first or last `n` rows in
`table_body`.

## Examples

``` r
# head() example
trial |>
  tbl_summary(by = trt) |>
  head(n = 2L)


  

Characteristic
```
