# Bold or Italicize

Bold or italicize labels or levels in gtsummary tables

## Usage

``` r
bold_labels(x)

italicize_labels(x)

bold_levels(x)

italicize_levels(x)

# S3 method for class 'gtsummary'
bold_labels(x)

# S3 method for class 'gtsummary'
bold_levels(x)

# S3 method for class 'gtsummary'
italicize_labels(x)

# S3 method for class 'gtsummary'
italicize_levels(x)

# S3 method for class 'tbl_cross'
bold_labels(x)

# S3 method for class 'tbl_cross'
bold_levels(x)

# S3 method for class 'tbl_cross'
italicize_labels(x)

# S3 method for class 'tbl_cross'
italicize_levels(x)
```

## Arguments

- x:

  (`gtsummary`) An object of class 'gtsummary'

## Value

Functions return the same class of gtsummary object supplied

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
tbl_summary(trial, include = c("trt", "age", "response")) |>
  bold_labels() |>
  bold_levels() |>
  italicize_labels() |>
  italicize_levels()


  

Characteristic
```
