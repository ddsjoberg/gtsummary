# Add CI Column

Add a new column with the confidence intervals for proportions, means,
etc.

## Usage

``` r
# S3 method for class 'tbl_svysummary'
add_ci(
  x,
  method = list(all_continuous() ~ "svymean", all_categorical() ~ "svyprop.logit"),
  include = everything(),
  statistic = list(all_continuous() ~ "{conf.low}, {conf.high}", all_categorical() ~
    "{conf.low}%, {conf.high}%"),
  conf.level = 0.95,
  style_fun = list(all_continuous() ~ label_style_sigfig(), all_categorical() ~
    label_style_sigfig(scale = 100)),
  pattern = NULL,
  df = survey::degf(x$inputs$data),
  ...
)
```

## Arguments

- x:

  (`tbl_summary`)  
  a summary table of class `'tblsummary'`

- method:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Confidence interval method. Default is
  `list(all_continuous() ~ "svymean", all_categorical() ~ "svyprop.logit")`.
  See details below.

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

- df:

  (`numeric`)  
  denominator degrees of freedom, passed to `survey::svyciprop(df)` or
  `confint(df)`. Default is `survey::degf(x$inputs$data)`.

- ...:

  These dots are for future extensions and must be empty.

## Value

gtsummary table

## method argument

Must be one of

- `"svyprop.logit"`, `"svyprop.likelihood"`, `"svyprop.asin"`,
  `"svyprop.beta"`, `"svyprop.mean"`, `"svyprop.xlogit"` calculated via
  [`survey::svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html)
  for **categorical** variables

- `"svymean"` calculated via
  [`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
  for **continuous** variables

- `"svymedian.mean"`, `"svymedian.beta"`, `"svymedian.xlogit"`,
  `"svymedian.asin"`, `"svymedian.score"` calculated via
  `survey::svyquantile(quantiles = 0.5)` for **continuous** variables

## Examples

``` r
data(api, package = "survey")
survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
  tbl_svysummary(
    by = "both",
    include = c(api00, stype),
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) |>
  add_stat_label() |>
  add_ci(pattern = "{stat} (95% CI {ci})") |>
  modify_header(all_stat_cols() ~ "**{level}**") |>
  modify_spanning_header(all_stat_cols() ~ "**Survived**")


  


Characteristic
```

**Survived**

**No**

**Yes**

api00, Mean (SD)

633 (96) (95% CI 570, 696)

648 (109) (95% CI 600, 697)

stype, n (%)

  

  

    E

1,083 (64%) (95% CI 43%, 81%)

3,791 (84%) (95% CI 73%, 91%)

    H

237 (14%) (95% CI 6.6%, 27%)

237 (5.3%) (95% CI 2.1%, 13%)

    M

372 (22%) (95% CI 8.7%, 46%)

474 (11%) (95% CI 5.7%, 19%)

Abbreviation: CI = Confidence Interval
