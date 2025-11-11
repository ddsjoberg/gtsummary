# Convert Named List to Table Body

Many arguments in 'gtsummary' accept named lists. This function converts
a named list to the `.$table_body` format expected in
[`scope_table_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/scoping_gtsummary.md)

## Usage

``` r
.list2tb(x, colname = caller_arg(x))
```

## Arguments

- x:

  named list

- colname:

  string of column name to assign. Default is `caller_arg(x)`

## Value

`.$table_body` data frame

## Examples

``` r
type <- list(age = "continuous", response = "dichotomous")
gtsummary:::.list2tb(type, "var_type")
#> # A tibble: 2 × 2
#>   variable var_type   
#>   <chr>    <chr>      
#> 1 age      continuous 
#> 2 response dichotomous
```
