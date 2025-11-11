# Add CI Column

Add a new column with the confidence intervals for proportions, means,
etc.

## Usage

``` r
add_ci(x, ...)

# S3 method for class 'tbl_summary'
add_ci(
  x,
  method = list(all_continuous() ~ "t.test", all_categorical() ~ "wilson"),
  include = everything(),
  statistic = list(all_continuous() ~ "{conf.low}, {conf.high}", all_categorical() ~
    "{conf.low}%, {conf.high}%"),
  conf.level = 0.95,
  style_fun = list(all_continuous() ~ label_style_sigfig(), all_categorical() ~
    label_style_sigfig(scale = 100)),
  pattern = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_summary`)  
  a summary table of class `'tblsummary'`

- ...:

  These dots are for future extensions and must be empty.

- method:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Confidence interval method. Default is
  `list(all_continuous() ~ "t.test", all_categorical() ~ "wilson")`. See
  details below.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Indicates how the confidence interval will be displayed. Default is
  `list(all_continuous() ~ "{conf.low}, {conf.high}", all_categorical() ~ "{conf.low}%, {conf.high}%")`

- conf.level:

  (scalar `real`)  
  Confidence level. Default is `0.95`

- style_fun:

  (`function`)  
  Function to style upper and lower bound of confidence interval.
  Default is
  `list(all_continuous() ~ label_style_sigfig(), all_categorical() ~ label_style_sigfig(scale = 100))`.

- pattern:

  (`string`)  
  Indicates the pattern to use to merge the CI with the statistics cell.
  The default is NULL, where no columns are merged. The two columns that
  will be merged are the statistics column, represented by `"{stat}"`
  and the CI column represented by `"{ci}"`, e.g.
  `pattern = "{stat} ({ci})"` will merge the two columns with the CI in
  parentheses. Default is `NULL`, and no merging is performed.

## Value

gtsummary table

## method argument

Must be one of

- `"wilson"`, `"wilson.no.correct"` calculated via
  `prop.test(correct = c(TRUE, FALSE))` for **categorical** variables

- `"exact"` calculated via
  [`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html) for
  **categorical** variables

- `"wald"`, `"wald.no.correct"` calculated via
  `cardx::proportion_ci_wald(correct = c(TRUE, FALSE)` for
  **categorical** variables

- `"agresti.coull"` calculated via
  [`cardx::proportion_ci_agresti_coull()`](https://insightsengineering.github.io/cardx/latest-tag/reference/proportion_ci.html)
  for **categorical** variables

- `"jeffreys"` calculated via
  [`cardx::proportion_ci_jeffreys()`](https://insightsengineering.github.io/cardx/latest-tag/reference/proportion_ci.html)
  for **categorical** variables

- `"t.test"` calculated via
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) for
  **continuous** variables

- `"wilcox.test"` calculated via
  [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) for
  **continuous** variables

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_summary(
    missing = "no",
    statistic = all_continuous() ~ "{mean} ({sd})",
    include = c(marker, response, trt)
  ) |>
  add_ci()


  

Characteristic
```

**N = 200**¹

**95% CI**

Marker Level (ng/mL)

0.92 (0.86)

0.79, 1.0

Tumor Response

61 (32%)

25%, 39%

Chemotherapy Treatment

  

  

    Drug A

98 (49%)

42%, 56%

    Drug B

102 (51%)

44%, 58%

¹ Mean (SD); n (%)

Abbreviation: CI = Confidence Interval

\# Example 2 ---------------------------------- trial \|\>
[select](https://dplyr.tidyverse.org/reference/select.html)(response,
grade) [%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(
statistic =
[all_categorical](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "{p}%", missing = "no", include =
[c](https://rdrr.io/r/base/c.html)(response, grade) ) \|\>
add_ci(pattern = "{stat} ({ci})") \|\>
[remove_footnote_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)([everything](https://tidyselect.r-lib.org/reference/everything.html)())

[TABLE]
