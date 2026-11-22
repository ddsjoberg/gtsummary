# Object Convert Helper

Ahead of a gtsummary object being converted to an output type, each
logical expression saved in `x$table_styling` is converted to a list of
row numbers.

## Usage

``` r
.table_styling_expr_to_row_number(x)
```

## Arguments

- x:

  a gtsummary object

## Value

a gtsummary object

## Examples

``` r
tbl <-
  trial %>%
  tbl_summary(include = c(age, grade)) %>%
  .table_styling_expr_to_row_number()
```
