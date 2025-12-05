# Is a date/time

`is_date_time()`: Predicate for date, time, or date-time vector
identification.

## Usage

``` r
is_date_time(x)
```

## Arguments

- x:

  a vector

## Value

a scalar logical

## Examples

``` r
iris |>
  dplyr::mutate(date = as.Date("2000-01-01") + dplyr::row_number()) |>
  lapply(gtsummary:::is_date_time)
#> $Sepal.Length
#> [1] FALSE
#> 
#> $Sepal.Width
#> [1] FALSE
#> 
#> $Petal.Length
#> [1] FALSE
#> 
#> $Petal.Width
#> [1] FALSE
#> 
#> $Species
#> [1] FALSE
#> 
#> $date
#> [1] TRUE
#> 
```
