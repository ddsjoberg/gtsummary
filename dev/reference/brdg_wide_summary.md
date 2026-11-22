# Wide summary table bridge

Bridge function for converting
[`tbl_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_wide_summary.md)
(and similar) cards to basic gtsummary objects. All bridge functions
begin with prefix `brdg_*()`.

The ARD data frame passed to this function must include a `"gts_column"`
column, which assigns each statistic to the column it will populate,
e.g. `"stat_1"` for statistics in the first element of each `statistic`
vector, `"stat_2"` for the second, etc.

## Usage

``` r
brdg_wide_summary(cards, variables, statistic, type)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- variables:

  (`character`)  
  character list of variables

- statistic:

  (named `list`)  
  named list of summary statistic names

- type:

  (named `list`)  
  named list of summary types

## Value

a gtsummary object

## Examples

``` r
library(cards)

bind_ard(
  ard_summary(trial, variables = c(age, marker)),
  ard_attributes(trial, variables = c(age, marker))
) |>
  # this column is used by the `pier_*()` functions
  dplyr::mutate(
    gts_column = dplyr::case_when(
      stat_name == "mean" ~ "stat_1",
      stat_name == "sd" ~ "stat_2"
    )
  ) |>
  brdg_wide_summary(
    variables = c("age", "marker"),
    statistic = list(age = c("{mean}", "{sd}"), marker = c("{mean}", "{sd}")),
    type = list(age = "continuous", marker = "continuous")
  )


  

Characteristic
```
