# Combine terms

The function combines terms from a regression model, and replaces the
terms with a single row in the output table. The p-value is calculated
using [`stats::anova()`](https://rdrr.io/r/stats/anova.html).

## Usage

``` r
combine_terms(x, formula_update, label = NULL, quiet, ...)
```

## Arguments

- x:

  (`tbl_regression`)  
  A `tbl_regression` object

- formula_update:

  (`formula`)  
  formula update passed to the
  [`stats::update()`](https://rdrr.io/r/stats/update.html). This updated
  formula is used to construct a reduced model, and is subsequently
  passed to [`stats::anova()`](https://rdrr.io/r/stats/anova.html) to
  calculate the p-value for the group of removed terms. See the
  [`stats::update()`](https://rdrr.io/r/stats/update.html) function's
  `formula.=` argument for proper syntax.

- label:

  (`string`)  
  Optional string argument labeling the combined rows

- quiet:

  **\[deprecated\]**

- ...:

  Additional arguments passed to
  [stats::anova](https://rdrr.io/r/stats/anova.html)

## Value

`tbl_regression` object

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
# Logistic Regression Example, LRT p-value
glm(response ~ marker + I(marker^2) + grade,
    trial[c("response", "marker", "grade")] |> na.omit(), # keep complete cases only!
    family = binomial) |>
  tbl_regression(label = grade ~ "Grade", exponentiate = TRUE) |>
  # collapse non-linear terms to a single row in output using anova
  combine_terms(
    formula_update = . ~ . - marker - I(marker^2),
    label = "Marker (non-linear terms)",
    test = "LRT"
  )


  

Characteristic
```

**OR**

**95% CI**

**p-value**

Marker (non-linear terms)

  

  

0.2

Grade

  

  

  

    I

—

—

  

    II

1.16

0.52, 2.55

0.7

    III

1.07

0.50, 2.30

0.9

Abbreviations: CI = Confidence Interval, OR = Odds Ratio
