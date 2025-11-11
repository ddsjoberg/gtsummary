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
syntax](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md)
used throughout gtsummary

## Examples

``` r
# Example 1 ----------------------------------
lm(age ~ grade + marker, trial) |>
  tbl_regression() |>
  add_vif()


  

Characteristic
```

**Beta**

**95% CI**

**p-value**

**GVIF**

**Adjusted GVIF**¹

Grade

  

  

  

1.0

1.0

    I

—

—

  

  

  

    II

0.64

-4.7, 6.0

0.8

  

  

    III

2.4

-2.8, 7.6

0.4

  

  

Marker Level (ng/mL)

-0.04

-2.6, 2.5

\>0.9

1.0

1.0

¹ GVIF^\[1/(2\*df)\]

Abbreviations: CI = Confidence Interval, GVIF = Generalized Variance
Inflation Factor

\# Example 2 ----------------------------------
[lm](https://rdrr.io/r/stats/lm.html)(age ~ grade + marker, trial) \|\>
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)()
\|\> add_vif([c](https://rdrr.io/r/base/c.html)("aGVIF", "df"))

[TABLE]
