# Wide ARD summary table

**\[experimental\]**  
This function is similar to
[`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md),
but places summary statistics wide, in separate columns. All included
variables must be of the same summary type, e.g. all continuous
summaries or all categorical summaries (which encompasses dichotomous
variables).

## Usage

``` r
tbl_ard_wide_summary(
  cards,
  statistic = switch(type[[1]], continuous = c("{median}", "{p25}, {p75}"), c("{n}",
    "{p}%")),
  type = NULL,
  label = NULL,
  value = NULL,
  include = everything()
)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- statistic:

  (`character`)  
  character vector of the statistics to present. Each element of the
  vector will result in a column in the summary table. Default is
  `c("{median}", "{p25}, {p75}")` for continuous summaries, and
  `c("{n}", "{p}%")` for categorical/dichotomous summaries

- type:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the summary type. Accepted value are
  `c("continuous", "continuous2", "categorical", "dichotomous")`. If not
  specified, default type is assigned via
  [`assign_summary_type()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/assign_summary_type.md).
  See below for details.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- value:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the level of a variable to display on a single row. The
  gtsummary type selectors, e.g.
  [`all_dichotomous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  cannot be used with this argument. Default is `NULL`. See below for
  details.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

## Value

a gtsummary table of class 'tbl_wide_summary'

## Examples

``` r
library(cards)

ard_stack(
  trial,
  ard_summary(variables = age),
  .missing = TRUE,
  .attributes = TRUE,
  .total_n = TRUE
) |>
  tbl_ard_wide_summary()


  

Characteristic
```

**Median**

**Q1, Q3**

Age

47.0

38.0, 57.0

[ard_stack](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html)(
trial,
[ard_tabulate_value](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate_value.html)(variables
= response),
[ard_tabulate](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate.html)(variables
= grade), .missing = TRUE, .attributes = TRUE, .total_n = TRUE ) \|\>
tbl_ard_wide_summary()

[TABLE]
