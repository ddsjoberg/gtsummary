# Wide summary table

This function is similar to
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
but places summary statistics wide, in separate columns. All included
variables must be of the same summary type, e.g. all continuous
summaries or all categorical summaries (which encompasses dichotomous
variables).

## Usage

``` r
tbl_wide_summary(
  data,
  label = NULL,
  statistic = switch(type[[1]], continuous = c("{median}", "{p25}, {p75}"), c("{n}",
    "{p}%")),
  digits = NULL,
  type = NULL,
  value = NULL,
  sort = all_categorical(FALSE) ~ "alphanumeric",
  include = everything()
)
```

## Arguments

- data:

  (`data.frame`)  
  A data frame.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- statistic:

  (`character`)  
  character vector of the statistics to present. Each element of the
  vector will result in a column in the summary table. Default is
  `c("{median}", "{p25}, {p75}")` for continuous summaries, and
  `c("{n}", "{p}%")` for categorical/dichotomous summaries

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

- sort:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies sorting to perform for categorical variables. Values must be
  one of `c("alphanumeric", "frequency")`. Default is
  `all_categorical(FALSE) ~ "alphanumeric"`.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

## Value

a gtsummary table of class 'tbl_wide_summary'

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_wide_summary(include = c(response, grade))


  

Characteristic
```

**n**

**%**

Tumor Response

61

32%

Grade

  

  

    I

68

34%

    II

68

34%

    III

64

32%

\# Example 2 ---------------------------------- trial \|\>
[tbl_strata](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md)(
strata = trt, ~tbl_wide_summary(.x, include =
[c](https://rdrr.io/r/base/c.html)(age, marker)) )

[TABLE]
