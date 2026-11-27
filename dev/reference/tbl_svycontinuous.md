# Summarize continuous variable from a survey object

**\[experimental\]**  
Summarize a continuous variable by one or more categorical variables,
taking into account survey weights and design.

## Usage

``` r
tbl_svycontinuous(
  data,
  variable,
  include = everything(),
  digits = NULL,
  by = NULL,
  statistic = everything() ~ "{median} ({p25}, {p75})",
  label = NULL,
  value = NULL
)
```

## Arguments

- data:

  (`survey.design`)  
  A survey object created with created with
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)

- variable:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single column from `data`. Variable name of the continuous column to
  be summarized.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- digits:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies how summary statistics are rounded. Values may be either
  integer(s) or function(s). If not specified, default formatting is
  assigned via
  [`assign_summary_digits()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/assign_summary_digits.md).
  See below for details.

- by:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single column from `data`. Summary statistics will be stratified by
  this variable. Default is `NULL`.

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies summary statistics to display for each variable. The default
  is `everything() ~ "{median} ({p25}, {p75})"`. See below for details.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- value:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Supply a value to display a variable on a single row, printing the
  results for the variable associated with the value (similar to a
  `'dichotomous'` display in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)).

## Value

a gtsummary table of class `'tbl_svycontinuous'`

## statistic argument

The statistic argument specifies the statistics presented in the table.
The input is a list of formulas that specify the statistics to report.
For example, `statistic = list(grade ~ "{mean} ({sd})")` would report
the mean and standard deviation of the continuous variable within the
levels of `grade`. A statistic name that appears between curly brackets
will be replaced with the numeric statistic (see
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)).

The following statistics are available to display.

- `{median}` median

- `{mean}` mean

- `{mean.std.error}` standard error of the sample mean computed with
  [`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)

- `{deff}` design effect of the sample mean computed with
  [`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)

- `{sd}` standard deviation

- `{var}` variance

- `{min}` minimum

- `{max}` maximum

- `{p##}` any integer percentile, where `##` is an integer from 0 to 100

- `{sum}` sum

Unlike
[`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md),
it is not possible to pass a custom function.

Statistics on the number of missing and non-missing observations of the
continuous variable and their proportions are also available to display.

- `{N_obs}` total number of observations

- `{N_miss}` number of missing observations

- `{N_nonmiss}` number of non-missing observations

- `{p_miss}` percentage of observations missing

- `{p_nonmiss}` percentage of observations not missing

- `{N_obs_unweighted}` unweighted total number of observations

- `{N_miss_unweighted}` unweighted number of missing observations

- `{N_nonmiss_unweighted}` unweighted number of non-missing observations

- `{p_miss_unweighted}` unweighted percentage of observations missing

- `{p_nonmiss_unweighted}` unweighted percentage of observations not
  missing

## Examples

``` r
# Example 1 ----------------------------------
survey::svydesign(~1, data = trial, weights = ~1) |>
  tbl_svycontinuous(
    variable = age,
    by = trt,
    include = grade
  )


  

Characteristic
```
