# Bold significant p-values

Bold values below a chosen threshold (e.g. \<0.05) in a gtsummary
tables.

## Usage

``` r
bold_p(x, t = 0.05, q = FALSE)
```

## Arguments

- x:

  (`gtsummary`)  
  Object created using gtsummary functions

- t:

  (scalar `numeric`)  
  Threshold below which values will be bold. Default is 0.05.

- q:

  (scalar `logical`)  
  When `TRUE` will bold the q-value column rather than the p-value.
  Default is `FALSE`.

## Author

Daniel D. Sjoberg, Esther Drill

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
  add_p() |>
  bold_p(t = 0.1)


  

Characteristic
```
