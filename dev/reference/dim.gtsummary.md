# gtsummary table dimension

Returns the dimension of a gtsummary table, that is, the number of rows
and the number of un-hidden columns.

[`nrow()`](https://rdrr.io/r/base/nrow.html) calls
[`dim()`](https://rdrr.io/r/base/dim.html); therefore,
[`nrow()`](https://rdrr.io/r/base/nrow.html) will also work on gtsummary
tables.

## Usage

``` r
# S3 method for class 'gtsummary'
dim(x)
```

## Arguments

- x:

  (`gtsummary`)  
  a 'gtsummary' table

## Value

integer vector

## Examples

``` r
tbl <- tbl_summary(trial, include = age, by = trt)

dim(tbl)
#> [1] 2 3
nrow(tbl)
#> [1] 2
```
