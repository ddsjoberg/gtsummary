# Add N

For each [`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
object summarized with
[`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
this function will add the total number of observations in a new column.

## Usage

``` r
# S3 method for class 'tbl_survfit'
add_n(x, ...)
```

## Arguments

- x:

  object of class "`tbl_survfit`"

- ...:

  Not used

## Examples

``` r
library(survival)
fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)

# Example 1 ----------------------------------
list(fit1, fit2) |>
  tbl_survfit(times = c(12, 24)) |>
  add_n()


  

Characteristic
```

**N**

**Time 12**

**Time 24**

Overall

200

89% (84%, 93%)

44% (38%, 51%)

Chemotherapy Treatment

200

  

  

    Drug A

  

91% (85%, 97%)

47% (38%, 58%)

    Drug B

  

86% (80%, 93%)

41% (33%, 52%)
