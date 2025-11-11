# Report statistics from cross table inline

Extracts and returns statistics from a `tbl_cross` object for inline
reporting in an R markdown document. Detailed examples in the
[inline_text
vignette](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)

## Usage

``` r
# S3 method for class 'tbl_cross'
inline_text(
  x,
  col_level,
  row_level = NULL,
  pvalue_fun = label_style_pvalue(prepend_p = TRUE),
  ...
)
```

## Arguments

- x:

  (`tbl_cross`)  
  A `tbl_cross` object

- col_level:

  (`string`)  
  Level of the column variable to display. Can also specify "`p.value`"
  for the p-value and "`stat_0`" for Total column.

- row_level:

  (`string`)  
  Level of the row variable to display.

- pvalue_fun:

  (`function`)  
  Function to round and format p-values. Default is
  [`label_style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md).
  The function must have a numeric vector input, and return a string
  that is the rounded/formatted p-value (e.g.
  `pvalue_fun = label_style_pvalue(digits = 2)`).

- ...:

  These dots are for future extensions and must be empty.

## Value

A string reporting results from a gtsummary table

## Examples

``` r
tbl_cross <-
  tbl_cross(trial, row = trt, col = response) %>%
  add_p()

inline_text(tbl_cross, row_level = "Drug A", col_level = "1")
#> [1] "28"
inline_text(tbl_cross, row_level = "Total", col_level = "1")
#> [1] "61"
inline_text(tbl_cross, col_level = "p.value")
#> [1] "p=0.7"
```
