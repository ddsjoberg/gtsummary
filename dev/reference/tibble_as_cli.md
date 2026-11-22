# Print tibble with cli

Print a tibble or data frame using cli styling and formatting.

## Usage

``` r
tibble_as_cli(x, na_value = "", label = list(), padding = 3L)
```

## Arguments

- x:

  (`data.frame`)  
  a data frame with all character columns.

- na_value:

  (`string`)  
  a string indicating how an `NA` value will appear in printed table.

- label:

  (named `list`)  
  named list of column labels to use. Default is to print the column
  names.

- padding:

  (`integer`)  
  an integer indicating the amount of padding between columns.

## Examples

``` r
trial[1:3, ] |>
  dplyr::mutate_all(as.character) |>
  gtsummary:::tibble_as_cli()
#> trt      age   marker   stage   grade   response   death   ttdeath   trt 
#> Drug A   23    0.16     T1      II      0          0       24        age 
#> Drug B   9     1.107    T2      I       1          0       24        marker 
#> Drug A   31    0.277    T1      II      0          0       24        stage 
```
