# Sort/filter by p-values

Sort/filter by p-values

## Usage

``` r
sort_p(x, q = FALSE)

filter_p(x, q = FALSE, t = 0.05)
```

## Arguments

- x:

  (`gtsummary`)  
  An object created using gtsummary functions

- q:

  (scalar `logical`)  
  When `TRUE` will check the q-value column rather than the p-value.
  Default is `FALSE`.

- t:

  (scalar `numeric`)  
  Threshold below which values will be retained. Default is 0.05.

## Author

Karissa Whiting, Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial %>%
  select(age, grade, response, trt) %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  filter_p(t = 0.8) %>%
  sort_p()


  

Characteristic
```
