# Wide summary table bridge

Bridge function for converting
[`tbl_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_wide_summary.md)
(and similar) cards to basic gtsummary objects. All bridge functions
begin with prefix `brdg_*()`.

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
  brdg_wide_summary(
    variables = c("age", "marker"),
    statistic = list(age = c("{mean}", "{sd}"), marker = c("{mean}", "{sd}")),
    type = list(age = "continuous", marker = "continuous")
  )


  

Characteristic
```

**Mean**

**SD**

Age

47.2

14.3

Marker Level (ng/mL)

0.9

0.9
