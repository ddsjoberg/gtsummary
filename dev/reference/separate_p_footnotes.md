# Create footnotes for individual p-values

**\[questioning\]**  
The usual presentation of footnotes for p-values on a gtsummary table is
to have a single footnote that lists all statistical tests that were
used to compute p-values on a given table. The `separate_p_footnotes()`
function separates aggregated p-value footnotes to individual footnotes
that denote the specific test used for each of the p-values.

## Usage

``` r
separate_p_footnotes(x)
```

## Arguments

- x:

  (`tbl_summary`, `tbl_svysummary`)  
  Object with class `"tbl_summary"` or `"tbl_svysummary"`

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p() |>
  separate_p_footnotes()


  

Characteristic
```
