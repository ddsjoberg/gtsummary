# Convert gtsummary object to a tibble

Function converts a gtsummary object to a tibble.

## Usage

``` r
# S3 method for class 'gtsummary'
as_tibble(
  x,
  include = everything(),
  col_labels = TRUE,
  return_calls = FALSE,
  fmt_missing = FALSE,
  ...
)

# S3 method for class 'gtsummary'
as.data.frame(...)
```

## Arguments

- x:

  (`gtsummary`)  
  An object of class `"gtsummary"`

- include:

  Commands to include in output. Input may be a vector of quoted or
  unquoted names. tidyselect and gtsummary select helper functions are
  also accepted. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- col_labels:

  (scalar `logical`)  
  Logical argument adding column labels to output tibble. Default is
  `TRUE`.

- return_calls:

  Logical. Default is `FALSE`. If `TRUE`, the calls are returned as a
  list of expressions.

- fmt_missing:

  (scalar `logical`)  
  Logical argument adding the missing value formats.

- ...:

  Arguments passed on to `gt::gt(...)`

## Value

a [tibble](https://tibble.tidyverse.org/)

## Author

Daniel D. Sjoberg

## Examples

``` r
tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, grade, response))

as_tibble(tbl)
#> # A tibble: 8 × 3
#>   `**Characteristic**` `**Drug A**  \nN = 98` `**Drug B**  \nN = 102`
#>   <chr>                <chr>                  <chr>                  
#> 1 Age                  46 (37, 60)            48 (39, 56)            
#> 2 Unknown              7                      4                      
#> 3 Grade                NA                     NA                     
#> 4 I                    35 (36%)               33 (32%)               
#> 5 II                   32 (33%)               36 (35%)               
#> 6 III                  31 (32%)               33 (32%)               
#> 7 Tumor Response       28 (29%)               33 (34%)               
#> 8 Unknown              3                      4                      

# without column labels
as_tibble(tbl, col_labels = FALSE)
#> # A tibble: 8 × 3
#>   label          stat_1      stat_2     
#>   <chr>          <chr>       <chr>      
#> 1 Age            46 (37, 60) 48 (39, 56)
#> 2 Unknown        7           4          
#> 3 Grade          NA          NA         
#> 4 I              35 (36%)    33 (32%)   
#> 5 II             32 (33%)    36 (35%)   
#> 6 III            31 (32%)    33 (32%)   
#> 7 Tumor Response 28 (29%)    33 (34%)   
#> 8 Unknown        3           4          
```
