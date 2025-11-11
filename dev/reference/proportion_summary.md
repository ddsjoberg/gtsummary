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

**Male**  
N = 16¹

**Female**  
N = 16¹

Age

  

  

    Child

45.3% (29/64) \[33-58\]

62.2% (28/45) \[47-76\]

    Adult

20.3% (338/1,667) \[18-22\]

74.4% (316/425) \[70-78\]

Class

  

  

    1st

34.4% (62/180) \[28-42\]

97.2% (141/145) \[93-99\]

    2nd

14.0% (25/179) \[9.4-20\]

87.7% (93/106) \[80-93\]

    3rd

17.3% (88/510) \[14-21\]

45.9% (90/196) \[39-53\]

    Crew

22.3% (192/862) \[20-25\]

87.0% (20/23) \[65-97\]

Overall

21.2% (367/1,731) \[19-23\]

73.2% (344/470) \[69-77\]

¹ Proportion (%) of survivors (n/N) \[95% CI\]
