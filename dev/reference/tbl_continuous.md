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
  is `everything() ~ "{median} ({p25}, {p75})"`.

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

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

Grade

  

  

    I

46 (36, 60)

48 (42, 55)

    II

45 (31, 55)

51 (42, 58)

    III

52 (42, 61)

45 (36, 52)

¹ Age: Median (Q1, Q3)

\# Example 2 ---------------------------------- trial \|\>
dplyr::[mutate](https://dplyr.tidyverse.org/reference/mutate.html)(all_subjects
= 1) \|\> tbl_continuous( variable = age, statistic = ~"{mean} ({sd})",
by = trt, include = [c](https://rdrr.io/r/base/c.html)(all_subjects,
stage, grade), value = all_subjects ~ 1, label =
[list](https://rdrr.io/r/base/list.html)(all_subjects = "All Subjects")
)

[TABLE]
