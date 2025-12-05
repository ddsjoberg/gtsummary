# Convert gtsummary object to a huxtable object

Function converts a gtsummary object to a huxtable object. A user can
use this function if they wish to add customized formatting available
via the huxtable functions. The huxtable package supports output to PDF
via LaTeX, as well as HTML and Word.

## Usage

``` r
as_hux_table(x, include = everything(), return_calls = FALSE)

as_hux_xlsx(x, file, include = everything(), bold_header_rows = TRUE)
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

- return_calls:

  Logical. Default is `FALSE`. If `TRUE`, the calls are returned as a
  list of expressions.

- file:

  File path for the output.

- bold_header_rows:

  (scalar `logical`)  
  logical indicating whether to bold header rows. Default is `TRUE`

## Value

A {huxtable} object

## Excel Output

Use the `as_hux_xlsx()` function to save a copy of the table in an excel
file. The file is saved using
[`huxtable::quick_xlsx()`](https://hughjonesd.github.io/huxtable/reference/quick-output.html).

## Author

David Hugh-Jones, Daniel D. Sjoberg

## Examples

``` r
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p() |>
  as_hux_table()
#>             Characteristic   Drug AN = 98   Drug BN = 102   p-value  
#>           ───────────────────────────────────────────────────────────
#>             Age              46 (37, 60)     48 (39, 56)      0.7    
#>             Unknown               7               4                  
#>             Grade                                             0.9    
#>             I                  35 (36%)       33 (32%)               
#>             II                 32 (33%)       36 (35%)               
#>             III                31 (32%)       33 (32%)               
#>           ───────────────────────────────────────────────────────────
#>             Median (Q1, Q3); n (%)                                   
#>             Wilcoxon rank sum test; Pearson's Chi-squared            
#>             test                                                     
#> 
#> Column names: label, stat_1, stat_2, p.value
```
