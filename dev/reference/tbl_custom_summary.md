# Create a table of summary statistics using a custom summary function

**\[experimental\]**  
The `tbl_custom_summary()` function calculates descriptive statistics
for continuous, categorical, and dichotomous variables. This function is
similar to
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
but allows you to provide a custom function in charge of computing the
statistics (see Details).

## Usage

``` r
tbl_custom_summary(
  data,
  by = NULL,
  label = NULL,
  stat_fns,
  statistic,
  digits = NULL,
  type = NULL,
  value = NULL,
  missing = c("ifany", "no", "always"),
  missing_text = "Unknown",
  missing_stat = "{N_miss}",
  include = everything(),
  overall_row = FALSE,
  overall_row_last = FALSE,
  overall_row_label = "Overall"
)
```

## Arguments

- data:

  (`data.frame`)  
  A data frame.

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

- stat_fns:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the function to be used to compute the statistics (see below
  for details and examples). You can also use dedicated helpers such as
  [`ratio_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/ratio_summary.md)
  or
  [`proportion_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/proportion_summary.md).

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

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- overall_row:

  (scalar `logical`)  
  Logical indicator to display an overall row. Default is `FALSE`. Use
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  to add an overall column.

- overall_row_last:

  (scalar `logical`)  
  Logical indicator to display overall row last in table. Default is
  `FALSE`, which will display overall row first.

- overall_row_label:

  (`string`)  
  String indicating the overall row label. Default is `"Overall"`.

## Value

A `tbl_custom_summary` object

## Similarities with [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)

Please refer to the help file of
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
regarding the use of select helpers, and arguments `include`, `by`,
`type`, `value`, `digits`, `missing` and `missing_text`.

## `stat_fns` argument

The `stat_fns` argument specify the custom function(s) to be used for
computing the summary statistics. For example,
`stat_fns = everything() ~ foo`.

Each function may take the following arguments:
`foo(data, full_data, variable, by, type, ...)`

- `data=` is the input data frame passed to `tbl_custom_summary()`,
  subset according to the level of `by` or `variable` if any, excluding
  `NA` values of the current `variable`

- `full_data=` is the full input data frame passed to
  `tbl_custom_summary()`

- `variable=` is a string indicating the variable to perform the
  calculation on

- `by=` is a string indicating the by variable from
  `tbl_custom_summary=`, if present

- `type=` is a string indicating the type of variable (continuous,
  categorical, ...)

- `stat_display=` a string indicating the statistic to display (for the
  `statistic` argument, for that variable)

The user-defined does not need to utilize each of these inputs. It's
encouraged the user-defined function accept `...` as each of the
arguments *will* be passed to the function, even if not all inputs are
utilized by the user's function, e.g. `foo(data, ...)` (see examples).

The user-defined function should return a one row
[`dplyr::tibble()`](https://dplyr.tidyverse.org/reference/reexports.html)
with one column per summary statistics (see examples).

## statistic argument

The statistic argument specifies the statistics presented in the table.
The input is a list of formulas that specify the statistics to report.
For example, `statistic = list(age ~ "{mean} ({sd})")`. A statistic name
that appears between curly brackets will be replaced with the numeric
statistic (see
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)). All
the statistics indicated in the statistic argument should be returned by
the functions defined in the `stat_fns` argument.

When the summary type is `"continuous2"`, pass a vector of statistics.
Each element of the vector will result in a separate row in the summary
table.

For both categorical and continuous variables, statistics on the number
of missing and non-missing observations and their proportions are also
available to display.

- `{N_obs}` total number of observations

- `{N_miss}` number of missing observations

- `{N_nonmiss}` number of non-missing observations

- `{p_miss}` percentage of observations missing

- `{p_nonmiss}` percentage of observations not missing

Note that for categorical variables, `{N_obs}`, `{N_miss}` and
`{N_nonmiss}` refer to the total number, number missing and number non
missing observations in the denominator, not at each level of the
categorical variable.

It is recommended to use
[`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)
to properly describe the displayed statistics (see examples).

## Caution

The returned table is compatible with all `gtsummary` features
applicable to a `tbl_summary` object, like
[`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md),
[`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)
or
[`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md).

However, some of them could be inappropriate in such case. In
particular,
[`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
do not take into account the type of displayed statistics and always
return the p-value of a comparison test of the current variable
according to the `by` groups, which may be incorrect if the displayed
statistics refer to a third variable.

## Author

Joseph Larmarange

## Examples

``` r
# Example 1 ----------------------------------
my_stats <- function(data, ...) {
  marker_sum <- sum(data$marker, na.rm = TRUE)
  mean_age <- mean(data$age, na.rm = TRUE)
  dplyr::tibble(
    marker_sum = marker_sum,
    mean_age = mean_age
  )
}

my_stats(trial)
#> # A tibble: 1 × 2
#>   marker_sum mean_age
#>        <dbl>    <dbl>
#> 1       174.     47.2

trial |>
  tbl_custom_summary(
    include = c("stage", "grade"),
    by = "trt",
    stat_fns = everything() ~ my_stats,
    statistic = everything() ~ "A: {mean_age} - S: {marker_sum}",
    digits = everything() ~ c(1, 0),
    overall_row = TRUE,
    overall_row_label = "All stages & grades"
  ) |>
  add_overall(last = TRUE) |>
  modify_footnote_header(
    footnote = "A: mean age - S: sum of marker",
    columns = all_stat_cols()
  ) |>
  bold_labels()


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**Overall**  
N = 200¹

All stages & grades

A: 47.0 - S: 94

A: 47.4 - S: 80

A: 47.2 - S: 174

T Stage

  

  

  

    T1

A: 44.1 - S: 19

A: 49.5 - S: 15

A: 46.8 - S: 35

    T2

A: 50.2 - S: 29

A: 46.4 - S: 29

A: 48.1 - S: 59

    T3

A: 48.8 - S: 21

A: 50.0 - S: 18

A: 49.4 - S: 39

    T4

A: 45.3 - S: 24

A: 44.3 - S: 18

A: 44.8 - S: 42

Grade

  

  

  

    I

A: 45.9 - S: 39

A: 46.4 - S: 31

A: 46.2 - S: 70

    II

A: 44.6 - S: 24

A: 50.3 - S: 19

A: 47.5 - S: 43

    III

A: 51.0 - S: 30

A: 45.7 - S: 30

A: 48.1 - S: 61

¹ A: mean age - S: sum of marker

\# Example 2 ---------------------------------- \# Use
\`data\[\[variable\]\]\` to access the current variable mean_ci \<-
function(data, variable, ...) { test \<-
[t.test](https://rdrr.io/r/stats/t.test.html)(data\[\[variable\]\])
dplyr::[tibble](https://tibble.tidyverse.org/reference/tibble.html)(
mean = test\$estimate, conf.low = test\$conf.int\[1\], conf.high =
test\$conf.int\[2\] ) } trial \|\> tbl_custom_summary( include =
[c](https://rdrr.io/r/base/c.html)("marker", "ttdeath"), by = "trt",
stat_fns = ~ mean_ci, statistic = ~ "{mean} \[{conf.low}; {conf.high}\]"
) \|\>
[add_overall](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)(last
= TRUE) \|\>
[modify_footnote_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)(
footnote = "mean \[95% CI\]", columns =
[all_stat_cols](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
)

[TABLE]

\# Example 3 ---------------------------------- \# Use \`full_data\` to
access the full datasets \# Returned statistic can also be a character
diff_to_great_mean \<- function(data, full_data, ...) { mean \<-
[mean](https://rdrr.io/r/base/mean.html)(data\$marker, na.rm = TRUE)
great_mean \<-
[mean](https://rdrr.io/r/base/mean.html)(full_data\$marker, na.rm =
TRUE) diff \<- mean - great_mean
dplyr::[tibble](https://tibble.tidyverse.org/reference/tibble.html)(
mean = mean, great_mean = great_mean, diff = diff, level =
[ifelse](https://rdrr.io/r/base/ifelse.html)(diff \> 0, "high", "low") )
} trial \|\> tbl_custom_summary( include =
[c](https://rdrr.io/r/base/c.html)("grade", "stage"), by = "trt",
stat_fns = ~ diff_to_great_mean, statistic = ~ "{mean} ({level}, diff:
{diff})", overall_row = TRUE ) \|\>
[bold_labels](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)()

[TABLE]
