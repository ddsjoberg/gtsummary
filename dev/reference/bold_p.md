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

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**p-value**²

Tumor Response

28 (29%)

33 (34%)

0.5

Marker Level (ng/mL)

0.84 (0.23, 1.60)

0.52 (0.18, 1.21)

0.085

¹ n (%); Median (Q1, Q3)

² Pearson’s Chi-squared test; Wilcoxon rank sum test

\# Example 2 ----------------------------------
[glm](https://rdrr.io/r/stats/glm.html)(response ~ trt + grade, trial,
family = [binomial](https://rdrr.io/r/stats/family.html)(link =
"logit")) \|\>
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)(exponentiate
= TRUE) \|\> bold_p(t = 0.65)

[TABLE]
