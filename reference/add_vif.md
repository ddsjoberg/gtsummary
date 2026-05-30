# Add Variance Inflation Factor

Add the variance inflation factor (VIF) or generalized VIF (GVIF) to the
regression table. Function uses
[`car::vif()`](https://rdrr.io/pkg/car/man/vif.html) to calculate the
VIF.

## Usage

``` r
add_vif(x, statistic = NULL, estimate_fun = label_style_sigfig(digits = 2))
```

## Arguments

- x:

  `'tbl_regression'` object

- statistic:

  `"VIF"` (variance inflation factors, for models with no categorical
  terms) or one of/combination of `"GVIF"` (generalized variance
  inflation factors), `"aGVIF"` 'adjusted GVIF, i.e. `GVIF^[1/(2*df)]`
  and/or `"df"` (degrees of freedom). See
  [`car::vif()`](https://rdrr.io/pkg/car/man/vif.html) for details.

- estimate_fun:

  Default is `label_style_sigfig(digits = 2)`.

## See also

Review [list, formula, and selector
syntax](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md)
used throughout gtsummary

## Examples

``` r
# Example 1 ----------------------------------
lm(age ~ grade + marker, trial) |>
  tbl_regression() |>
  add_vif()


  

Characteristic
```
