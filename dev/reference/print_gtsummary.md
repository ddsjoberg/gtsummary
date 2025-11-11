# print and knit_print methods for gtsummary objects

print and knit_print methods for gtsummary objects

## Usage

``` r
# S3 method for class 'gtsummary'
print(
  x,
  print_engine = c("gt", "flextable", "huxtable", "kable", "kable_extra", "tibble"),
  ...
)

# S3 method for class 'gtsummary'
knit_print(
  x,
  print_engine = c("gt", "flextable", "huxtable", "kable", "kable_extra", "tibble"),
  ...
)

pkgdown_print.gtsummary(x, visible = TRUE)
```

## Arguments

- x:

  An object created using gtsummary functions

- print_engine:

  String indicating the print method. Must be one of `"gt"`, `"kable"`,
  `"kable_extra"`, `"flextable"`, `"tibble"`

- ...:

  Not used

## Author

Daniel D. Sjoberg
