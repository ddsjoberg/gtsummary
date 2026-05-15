# Summarize a proportion

This helper, to be used with
[`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md),
creates a function computing a proportion and its confidence interval.

## Usage

``` r
proportion_summary(
  variable,
  value,
  weights = NULL,
  na.rm = TRUE,
  conf.level = 0.95,
  method = c("wilson", "wilson.no.correct", "wald", "wald.no.correct", "exact",
    "agresti.coull", "jeffreys")
)
```

## Arguments

- variable:

  (`string`)  
  String indicating the name of the variable from which the proportion
  will be computed.

- value:

  (`scalar`)  
  Value (or list of values) of `variable` to be taken into account in
  the numerator.

- weights:

  (`string`)  
  Optional string indicating the name of a frequency weighting variable.
  If `NULL`, all observations will be assumed to have a weight equal to
  `1`.

- na.rm:

  (scalar `logical`)  
  Should missing values be removed before computing the proportion?
  (default is `TRUE`)

- conf.level:

  (scalar `numeric`)  
  Confidence level for the returned confidence interval. Must be
  strictly greater than 0 and less than 1. Default to 0.95, which
  corresponds to a 95 percent confidence interval.

- method:

  (`string`)  
  Confidence interval method. Must be one of
  `c("wilson", "wilson.no.correct", "wald", "wald.no.correct", "exact", "agresti.coull", "jeffreys")`.
  See
  [`add_ci()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)
  for details.

## Details

Computed statistics:

- `{n}` numerator, number of observations equal to `values`

- `{N}` denominator, number of observations

- `{prop}` proportion, i.e. `n/N`

- `{conf.low}` lower confidence interval

- `{conf.high}` upper confidence interval

Methods `c("wilson", "wilson.no.correct")` are calculated with
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) (with
`correct = c(TRUE, FALSE)`). The default method, `"wilson"`, includes
the Yates continuity correction. Methods `c("exact", "asymptotic")` are
calculated with `Hmisc::binconf()` and the corresponding method.

## Author

Joseph Larmarange

## Examples

``` r
# Example 1 ----------------------------------
Titanic |>
  as.data.frame() |>
  tbl_custom_summary(
    include = c("Age", "Class"),
    by = "Sex",
    stat_fns = ~ proportion_summary("Survived", "Yes", weights = "Freq"),
    statistic = ~ "{prop}% ({n}/{N}) [{conf.low}-{conf.high}]",
    digits = ~ list(
      prop = label_style_percent(digits = 1),
      n = 0,
      N = 0,
      conf.low = label_style_percent(),
      conf.high = label_style_percent()
    ),
    overall_row = TRUE,
    overall_row_last = TRUE
  ) |>
  bold_labels() |>
  modify_footnote_header("Proportion (%) of survivors (n/N) [95% CI]", columns = all_stat_cols())


  

Characteristic
```
