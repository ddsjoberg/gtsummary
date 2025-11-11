# Continuous Summary Table Bridges

Bridge function for converting
[`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
cards to basic gtsummary objects. This bridge function converts the
'cards' object to a format suitable to pass to
[`brdg_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/brdg_summary.md):
no `pier_*()` functions required.

## Usage

``` r
brdg_continuous(cards, by = NULL, statistic, include, variable, type)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- by:

  (`string`)  
  string indicating the stratifying column

- statistic:

  (named `list`)  
  named list of summary statistic names

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- variable:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single column from `data`. Variable name of the continuous column to
  be summarized.

- type:

  (named `list`)  
  named list of summary types

## Value

a gtsummary object

## Examples

``` r
library(cards)

bind_ard(
  # the primary ARD with the results
  ard_summary(trial, by = grade, variables = age),
  # add missing and attributes ARD
  ard_missing(trial, by = grade, variables = age),
  ard_attributes(trial, variables = c(grade, age))
) |>
  # adding the column name
  dplyr::mutate(
    gts_column =
      ifelse(!context %in% "attributes", "stat_0", NA_character_)
  ) |>
  brdg_continuous(
    variable = "age",
    include = "grade",
    statistic = list(grade = "{median} ({p25}, {p75})"),
    type = list(grade = "categorical")
 ) |>
 as_tibble()
#> # A tibble: 4 × 2
#>   `**Characteristic**` stat_0           
#>   <chr>                <chr>            
#> 1 Grade                NA               
#> 2 I                    47.0 (37.0, 56.0)
#> 3 II                   48.5 (37.0, 57.0)
#> 4 III                  47.0 (38.0, 58.0)
```
