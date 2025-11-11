# Summarize the ratio of two variables

This helper, to be used with
[`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md),
creates a function computing the ratio of two continuous variables and
its confidence interval.

## Usage

``` r
ratio_summary(numerator, denominator, na.rm = TRUE, conf.level = 0.95)
```

## Arguments

- numerator:

  (`string`)  
  String indicating the name of the variable to be summed for computing
  the numerator.

- denominator:

  (`string`)  
  String indicating the name of the variable to be summed for computing
  the denominator.

- na.rm:

  (scalar `logical`)  
  Should missing values be removed before summing the numerator and the
  denominator? (default is `TRUE`)

- conf.level:

  (scalar `numeric`)  
  Confidence level for the returned confidence interval. Must be
  strictly greater than 0 and less than 1. Default to 0.95, which
  corresponds to a 95 percent confidence interval.

## Details

Computed statistics:

- `{num}` sum of the variable defined by `numerator`

- `{denom}` sum of the variable defined by `denominator`

- `{ratio}` ratio of `num` by `denom`

- `{conf.low}` lower confidence interval

- `{conf.high}` upper confidence interval

Confidence interval is computed with
[`stats::poisson.test()`](https://rdrr.io/r/stats/poisson.test.html), if
and only if `num` is an integer.

## Author

Joseph Larmarange

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_custom_summary(
    include = c("stage", "grade"),
    by = "trt",
    stat_fns = ~ ratio_summary("response", "ttdeath"),
    statistic = ~"{ratio} [{conf.low}; {conf.high}] ({num}/{denom})",
    digits = ~ c(ratio = 3, conf.low = 2, conf.high = 2),
    overall_row = TRUE,
    overall_row_label = "All stages & grades"
  ) |>
  bold_labels() |>
  modify_footnote_header("Ratio [95% CI] (n/N)", columns = all_stat_cols())


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

All stages & grades

0.014 \[0.01; 0.02\] (28.0000/1,983.0000)

0.017 \[0.01; 0.02\] (33.0000/1,941.6100)

T Stage

  

  

    T1

0.012 \[0.00; 0.02\] (7/583)

0.021 \[0.01; 0.04\] (11/522)

    T2

0.011 \[0.00; 0.02\] (6/528)

0.012 \[0.01; 0.03\] (7/560)

    T3

0.019 \[0.01; 0.04\] (8/426)

0.016 \[0.01; 0.03\] (7/425)

    T4

0.016 \[0.01; 0.03\] (7/445)

0.018 \[0.01; 0.04\] (8/434)

Grade

  

  

    I

0.011 \[0.00; 0.02\] (8/734)

0.019 \[0.01; 0.03\] (13/690)

    II

0.011 \[0.00; 0.02\] (7/651)

0.019 \[0.01; 0.03\] (12/645)

    III

0.022 \[0.01; 0.04\] (13/598)

0.013 \[0.01; 0.03\] (8/607)

¹ Ratio \[95% CI\] (n/N)
