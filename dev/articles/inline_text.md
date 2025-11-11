# inline_text() tutorial

## Introduction

**Reproducible reports** are an important part of good practices. We
often need to report the **results from a table** in the text of an R
markdown report. **Inline reporting** has been made simple with
[`inline_text()`](https://www.danieldsjoberg.com/gtsummary/reference/inline_text.tbl_summary.html).
The
[`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md)
function reports statistics from {gtsummary} tables inline in an [R
markdown](https://rmarkdown.rstudio.com/lesson-1.html) report.

## Setup

Before going through the tutorial, install and load {gtsummary}.

``` r
# install.packages("gtsummary")
library(gtsummary)
```

## Example data set

We’ll be using the
[`trial`](https://www.danieldsjoberg.com/gtsummary/reference/trial.html)
data set throughout this example.

- This set contains data from 200 patients who received one of two types
  of chemotherapy (Drug A or Drug B). The outcomes are tumor response
  and death.

## Inline results from tbl_summary()

First create a basic summary table using
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
(review [`tbl_summary()`
vignette](https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
for detailed overview of this function if needed).

``` r
tab1 <- tbl_summary(trial, by = trt, include = c(marker, stage))
tab1
```

[TABLE]

To report the median (IQR) of the marker levels in each group, use the
following commands inline.

> The median (IQR) marker level in the Drug A and Drug B groups are
> `` `r inline_text(tab1, variable = marker, column = "Drug A")` `` and
> `` `r inline_text(tab1, variable = marker, column = "Drug B")` ``,
> respectively.

Here’s how the line will appear in your report.

> The median (IQR) marker level in the Drug A and Drug B groups are 0.84
> (0.23, 1.60) and 0.52 (0.18, 1.21), respectively.

If you display a statistic from a categorical variable, include the
`level` argument.

> `` `r inline_text(tab1, variable = stage, level = "T1", column = "Drug B")` ``
> resolves to “25 (25%)”

## Inline results from tbl_regression()

Similar syntax is used to report results from
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
and
[`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_uvregression.html)
tables. Refer to the [`tbl_regression()`
vignette](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
if you need detailed guidance on using these functions.

Let’s first create a regression model.

``` r
# build logistic regression model
m1 <- glm(response ~ age + stage, data = trial, family = binomial(link = "logit"))
```

Now summarize the results with
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md);
exponentiate to get the odds ratios.

``` r
tbl_m1 <- tbl_regression(m1, exponentiate = TRUE)
tbl_m1
```

[TABLE]

To report the result for `age`, use the following commands inline.

> `` `r inline_text(tbl_m1, variable = age)` ``

Here’s how the line will appear in your report.

> 1.02 (95% CI 1.00, 1.04; p=0.091)

It is reasonable that you’ll need to modify the text. To do this, use
the `pattern` argument. The `pattern` argument syntax follows
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) format
with referenced R objects being inserted between curly brackets. The
default is
`pattern = "{estimate} ({conf.level*100}% CI {conf.low}, {conf.high}; {p.value})"`.
You have access the to following fields within the `pattern` argument.

| Parameter      | Description                                           |
|----------------|-------------------------------------------------------|
| `{estimate}`   | primary estimate (e.g. model coefficient, odds ratio) |
| `{conf.low}`   | lower limit of confidence interval                    |
| `{conf.high}`  | upper limit of confidence interval                    |
| `{p.value}`    | p-value                                               |
| `{conf.level}` | confidence level of interval                          |
| `{N}`          | number of observations                                |

> Age was not significantly associated with tumor response
> `` `r inline_text(tbl_m1, variable = age, pattern = "(OR {estimate}; 95% CI {conf.low}, {conf.high}; {p.value})")` ``.

If you’re printing results from a categorical variable, include the
`level` argument,
e.g. `inline_text(tbl_m1, variable = stage, level = "T3")` resolves to
“0.94 (95% CI 0.39, 2.28; p=0.9)”.

The `inline_text` function has arguments for rounding the p-value
(`pvalue_fun`) and the coefficients and confidence interval
(`estimate_fun`). These default to the same rounding performed in the
table, but can be modified when reporting inline.

For more details about inline code, review to the [RStudio documentation
page](https://rmarkdown.rstudio.com/lesson-4.html).
