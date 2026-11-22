# Select helper functions

Set of functions to supplement the {tidyselect} set of functions for
selecting columns of data frames (and other items as well).

- `all_continuous()` selects continuous variables

- `all_continuous2()` selects only type `"continuous2"`

- `all_categorical()` selects categorical (including `"dichotomous"`)
  variables

- `all_dichotomous()` selects only type `"dichotomous"`

- `all_tests()` selects variables by the name of the test performed

- `all_stat_cols()` selects columns from `tbl_summary`/`tbl_svysummary`
  object with summary statistics (i.e. `"stat_0"`, `"stat_1"`,
  `"stat_2"`, etc.)

- `all_interaction()` selects interaction terms from a regression model

- `all_intercepts()` selects intercept terms from a regression model

- `all_contrasts()` selects variables in regression model based on their
  type of contrast

## Usage

``` r
all_continuous(continuous2 = TRUE)

all_continuous2()

all_categorical(dichotomous = TRUE)

all_dichotomous()

all_tests(tests)

all_intercepts()

all_interaction()

all_contrasts(
  contrasts_type = c("treatment", "sum", "poly", "helmert", "sdif", "other")
)

all_stat_cols(stat_0 = TRUE)
```

## Arguments

- continuous2:

  (scalar `logical`)  
  Logical indicating whether to include continuous2 variables. Default
  is `TRUE`

- dichotomous:

  (scalar `logical`)  
  Logical indicating whether to include dichotomous variables. Default
  is `TRUE`

- tests:

  (`character`)  
  character vector indicating the test type of the variables to select,
  e.g. select all variables being compared with `"t.test"`.

- contrasts_type:

  (`character`)  
  type of contrast to select. Select among contrast types
  `c("treatment", "sum", "poly", "helmert", "sdif", "other")`. Default
  is all contrast types.

- stat_0:

  (scalar `logical`)  
  When `FALSE`, will not select the `"stat_0"` column. Default is `TRUE`

## Value

A character vector of column names selected

## See also

Review [list, formula, and selector
syntax](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md)
used throughout gtsummary

## Examples

``` r
select_ex1 <-
  trial |>
  select(age, response, grade) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_dichotomous() ~ "categorical"
  )
```
