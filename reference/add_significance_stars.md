# Add significance stars

Add significance stars to estimates with small p-values

## Usage

``` r
add_significance_stars(
  x,
  pattern = ifelse(inherits(x, c("tbl_regression", "tbl_uvregression")),
    "{estimate}{stars}", "{p.value}{stars}"),
  thresholds = c(0.001, 0.01, 0.05),
  hide_ci = TRUE,
  hide_p = inherits(x, c("tbl_regression", "tbl_uvregression")),
  hide_se = FALSE
)
```

## Arguments

- x:

  (`gtsummary`)  
  A `'gtsummary'` object with a `'p.value'` column

- pattern:

  (`string`)  
  glue-syntax string indicating what to display in formatted column.
  Default is `"{estimate}{stars}"` for regression summaries and
  `"{p.value}{stars}"` otherwise. A footnote is placed on the first
  column listed in the pattern. Other common patterns are
  `"{estimate}{stars} ({conf.low}, {conf.high})"` and
  `"{estimate} ({conf.low} to {conf.high}){stars}"`

- thresholds:

  (`numeric`)  
  Thresholds for significance stars. Default is `c(0.001, 0.01, 0.05)`

- hide_ci:

  (scalar `logical`)  
  logical whether to hide confidence interval. Default is `TRUE`

- hide_p:

  (scalar `logical`)  
  logical whether to hide p-value. Default is `TRUE` for regression
  summaries, and `FALSE` otherwise.

- hide_se:

  (scalar `logical`)  
  logical whether to hide standard error. Default is `FALSE`

## Value

a 'gtsummary' table

## Examples

``` r
tbl <-
  lm(time ~ ph.ecog + sex, survival::lung) |>
  tbl_regression(label = list(ph.ecog = "ECOG Score", sex = "Sex"))

# Example 1 ----------------------------------
tbl |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)


  

Characteristic
```
