# Summarize continuous variable

Summarize a continuous variable by one or more categorical variables

## Usage

``` r
tbl_continuous(
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

  (`data.frame`)  
  A data frame.

- variable:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single column from `data`. Variable name of the continuous column to
  be summarized.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- digits:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Specifies how summary statistics are rounded. Values may be either
  integer(s) or function(s). If not specified, default formatting is
  assigned via
  [`assign_summary_digits()`](https://www.danieldsjoberg.com/gtsummary/reference/assign_summary_digits.md).
  See below for details.

- by:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single column from `data`. Summary statistics will be stratified by
  this variable. Default is `NULL`.

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Specifies summary statistics to display for each variable. The default
  is `everything() ~ "{median} ({p25}, {p75})"`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- value:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Supply a value to display a variable on a single row, printing the
  results for the variable associated with the value (similar to a
  `'dichotomous'` display in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.md)).

## Value

a gtsummary table

## Examples

``` r
# Example 1 ----------------------------------
tbl_continuous(
  data = trial,
  variable = age,
  by = trt,
  include = grade
)


  

Characteristic
```
