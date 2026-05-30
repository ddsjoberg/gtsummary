# Summarize continuous variable

**\[experimental\]**  
Summarize a continuous variable by one or more categorical variables

## Usage

``` r
tbl_ard_continuous(
  cards,
  variable,
  include,
  by = NULL,
  label = NULL,
  statistic = everything() ~ "{median} ({p25}, {p75})",
  value = NULL
)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- variable:

  (`string`)  
  A single variable name of the continuous variable being summarized.

- include:

  (`character`)  
  Character vector of the categorical variables to

- by:

  (`string`)  
  A single variable name of the stratifying variable.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Specifies summary statistics to display for each variable. The default
  is `everything() ~ "{median} ({p25}, {p75})"`.

- value:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  Supply a value to display a variable on a single row, printing the
  results for the variable associated with the value (similar to a
  `'dichotomous'` display in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.md)).

## Value

a gtsummary table of class `"tbl_ard_summary"`

## Examples

``` r
library(cards)

# Example 1 ----------------------------------
# the primary ARD with the results
ard_summary(
  # the order variables are passed is important for the `by` variable.
  # 'trt' is the column stratifying variable and needs to be listed first.
  trial, by = c(trt, grade), variables = age
) |>
  # adding OPTIONAL information about the summary variables
  bind_ard(
    # add univariate trt tabulation
    ard_tabulate(trial, variables = trt),
    # add missing and attributes ARD
    ard_missing(trial, by = c(trt, grade), variables = age),
    ard_attributes(trial, variables = c(trt, grade, age))
  ) |>
  tbl_ard_continuous(by = "trt", variable = "age", include = "grade")


  

Characteristic
```
