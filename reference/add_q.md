# Add multiple comparison adjustment

Adjustments to p-values are performed with
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

## Usage

``` r
add_q(x, method = "fdr", pvalue_fun = NULL, quiet = NULL)
```

## Arguments

- x:

  (`gtsummary`)  
  a `gtsummary` object with a column named `"p.value"`

- method:

  (`string`)  
  String indicating method to be used for p-value adjustment. Methods
  from [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) are
  accepted. Default is `method='fdr'`. Must be one of 'holm',
  'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none'

- pvalue_fun:

  (`function`)  
  Function to round and format q-values. Default is the function
  specified to round the existing `'p.value'` column.

- quiet:

  **\[deprecated\]**

## Author

Daniel D. Sjoberg, Esther Drill

## Examples

``` r
# Example 1 ----------------------------------
add_q_ex1 <-
  trial |>
  tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
  add_p() |>
  add_q()

# Example 2 ----------------------------------
trial |>
  tbl_uvregression(
    y = response,
    include = c("trt", "age", "grade"),
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |>
  add_global_p() |>
  add_q()


  

Characteristic
```

**N**

**OR**

**95% CI**

**p-value**

**q-value**¹

Chemotherapy Treatment

193

  

  

0.5

0.8

    Drug A

  

—

—

  

  

    Drug B

  

1.21

0.66, 2.24

  

  

Age

183

1.02

1.00, 1.04

0.091

0.3

Grade

193

  

  

\>0.9

\>0.9

    I

  

—

—

  

  

    II

  

0.95

0.45, 2.00

  

  

    III

  

1.10

0.52, 2.29

  

  

¹ False discovery rate correction for multiple testing

Abbreviations: CI = Confidence Interval, OR = Odds Ratio
