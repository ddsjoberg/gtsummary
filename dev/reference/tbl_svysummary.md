# Create a table of summary statistics from a survey object

The `tbl_svysummary()` function calculates descriptive statistics for
continuous, categorical, and dichotomous variables taking into account
survey weights and design.

## Usage

``` r
tbl_svysummary(
  data,
  by = NULL,
  label = NULL,
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~
    "{n} ({p}%)"),
  digits = NULL,
  type = NULL,
  value = NULL,
  missing = c("ifany", "no", "always"),
  missing_text = "Unknown",
  missing_stat = "{N_miss}",
  sort = all_categorical(FALSE) ~ "alphanumeric",
  percent = c("column", "row", "cell"),
  include = everything()
)
```

## Arguments

- data:

  (`survey.design`)  
  A survey object created with created with
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)

- by:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single column from `data`. Summary statistics will be stratified by
  this variable. Default is `NULL`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies summary statistics to display for each variable. The default
  is
  `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
  See below for details.

- digits:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies how summary statistics are rounded. Values may be either
  integer(s) or function(s). If not specified, default formatting is
  assigned via
  [`assign_summary_digits()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/assign_summary_digits.md).
  See below for details.

- type:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the summary type. Accepted value are
  `c("continuous", "continuous2", "categorical", "dichotomous")`. If not
  specified, default type is assigned via
  [`assign_summary_type()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/assign_summary_type.md).
  See below for details.

- value:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the level of a variable to display on a single row. The
  gtsummary type selectors, e.g.
  [`all_dichotomous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  cannot be used with this argument. Default is `NULL`. See below for
  details.

- missing, missing_text, missing_stat:

  Arguments dictating how and if missing values are presented:

  - `missing`: must be one of `c("ifany", "no", "always")`.

  - `missing_text`: string indicating text shown on missing row. Default
    is `"Unknown"`.

  - `missing_stat`: statistic to show on missing row. Default is
    `"{N_miss}"`. Possible values are `N_miss`, `N_obs`, `N_nonmiss`,
    `p_miss`, `p_nonmiss`.

- sort:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies sorting to perform for categorical variables. Values must be
  one of `c("alphanumeric", "frequency")`. Default is
  `all_categorical(FALSE) ~ "alphanumeric"`.

- percent:

  (`string`)  
  Indicates the type of percentage to return. Must be one of
  `c("column", "row", "cell")`. Default is `"column"`.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

## Value

A `'tbl_svysummary'` object

## statistic argument

The statistic argument specifies the statistics presented in the table.
The input is a list of formulas that specify the statistics to report.
For example, `statistic = list(age ~ "{mean} ({sd})")` would report the
mean and standard deviation for age;
`statistic = list(all_continuous() ~ "{mean} ({sd})")` would report the
mean and standard deviation for all continuous variables. A statistic
name that appears between curly brackets will be replaced with the
numeric statistic (see
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)).

For categorical variables the following statistics are available to
display.

- `{n}` frequency

- `{N}` denominator, or cohort size

- `{p}` proportion

- `{p.std.error}` standard error of the sample proportion (on the 0 to 1
  scale) computed with
  [`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)

- `{deff}` design effect of the sample proportion computed with
  [`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)

- `{n_unweighted}` unweighted frequency

- `{N_unweighted}` unweighted denominator

- `{p_unweighted}` unweighted formatted percentage

For continuous variables the following statistics are available to
display.

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
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
it is not possible to pass a custom function.

For both categorical and continuous variables, statistics on the number
of missing and non-missing observations and their proportions are
available to display.

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

Note that for categorical variables, `{N_obs}`, `{N_miss}` and
`{N_nonmiss}` refer to the total number, number missing and number non
missing observations in the denominator, not at each level of the
categorical variable.

## type and value arguments

There are four summary types. Use the `type` argument to change the
default summary types.

- `"continuous"` summaries are shown on a *single row*. Most numeric
  variables default to summary type continuous.

- `"continuous2"` summaries are shown on *2 or more rows*

- `"categorical"` *multi-line* summaries of nominal data. Character
  variables, factor variables, and numeric variables with fewer than 10
  unique levels default to type categorical. To change a numeric
  variable to continuous that defaulted to categorical, use
  `type = list(varname ~ "continuous")`

- `"dichotomous"` categorical variables that are displayed on a *single
  row*, rather than one row per level of the variable. Variables coded
  as `TRUE`/`FALSE`, `0`/`1`, or `yes`/`no` are assumed to be
  dichotomous, and the `TRUE`, `1`, and `yes` rows are displayed.
  Otherwise, the value to display must be specified in the `value`
  argument, e.g. `value = list(varname ~ "level to show")`

## Author

Joseph Larmarange

## Examples

``` r
# Example 1 ----------------------------------
survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
  tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))


  

Characteristic
```

**No**  
N = 1,490¹

**Yes**  
N = 711¹

Class

  

  

    1st

122 (38%)

203 (62%)

    2nd

167 (59%)

118 (41%)

    3rd

528 (75%)

178 (25%)

    Crew

673 (76%)

212 (24%)

Age

  

  

    Child

52 (48%)

57 (52%)

    Adult

1,438 (69%)

654 (31%)

¹ n (%)

\# Example 2 ---------------------------------- \# A dataset with a
complex design [data](https://rdrr.io/r/utils/data.html)(api, package =
"survey")
survey::[svydesign](https://rdrr.io/pkg/survey/man/svydesign.html)(id =
~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) \|\>
tbl_svysummary(by = "both", include =
[c](https://rdrr.io/r/base/c.html)(api00, stype)) \|\>
[modify_spanning_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)([all_stat_cols](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "\*\*Survived\*\*")

[TABLE]
