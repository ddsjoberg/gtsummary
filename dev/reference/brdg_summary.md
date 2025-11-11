# Summary table bridge

Bridge function for converting
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
(and similar) cards to basic gtsummary objects. All bridge functions
begin with prefix `brdg_*()`.

This file also contains helper functions for constructing the bridge,
referred to as the piers (supports for a bridge) and begin with
`pier_*()`.

- `brdg_summary()`: The bridge function ingests an ARD data frame and
  returns a gtsummary table that includes `.$table_body` and a basic
  `.$table_styling`. The `.$table_styling$header` data frame includes
  the header statistics. Based on context, this function adds a column
  to the ARD data frame named `"gts_column"`. This column is used during
  the reshaping in the `pier_*()` functions defining column names.

- `pier_*()`: these functions accept a cards tibble and returns a tibble
  that is a piece of the `.$table_body`. Typically these will be stacked
  to construct the final table body data frame. The ARD object passed
  here will have two primary parts: the calculated summary statistics
  and the attributes ARD. The attributes ARD is used for labeling. The
  ARD data frame passed to this function must include a `"gts_column"`
  column, which is added in `brdg_summary()`.

## Usage

``` r
brdg_summary(
  cards,
  variables,
  type,
  statistic,
  by = NULL,
  missing = "no",
  missing_stat = "{N_miss}",
  missing_text = "Unknown"
)

pier_summary_dichotomous(cards, variables, statistic)

pier_summary_categorical(cards, variables, statistic)

pier_summary_continuous2(cards, variables, statistic)

pier_summary_continuous(cards, variables, statistic)

pier_summary_missing_row(
  cards,
  variables,
  missing = "no",
  missing_stat = "{N_miss}",
  missing_text = "Unknown"
)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- variables:

  (`character`)  
  character list of variables

- type:

  (named `list`)  
  named list of summary types

- statistic:

  (named `list`)  
  named list of summary statistic names

- by:

  (`string`)  
  string indicating the stratifying column

- missing, missing_text, missing_stat:

  Arguments dictating how and if missing values are presented:

  - `missing`: must be one of `c("ifany", "no", "always")`.

  - `missing_text`: string indicating text shown on missing row. Default
    is `"Unknown"`.

  - `missing_stat`: statistic to show on missing row. Default is
    `"{N_miss}"`. Possible values are `N_miss`, `N_obs`, `N_nonmiss`,
    `p_miss`, `p_nonmiss`.

## Value

a gtsummary object

## Examples

``` r
library(cards)

# first build ARD data frame
cards <-
  ard_stack(
    mtcars,
    ard_summary(variables = c("mpg", "hp")),
    ard_tabulate(variables = "cyl"),
    ard_tabulate_value(variables = "am"),
    .missing = TRUE,
    .attributes = TRUE
  ) |>
  # this column is used by the `pier_*()` functions
  dplyr::mutate(gts_column = ifelse(context == "attributes", NA, "stat_0"))

brdg_summary(
  cards = cards,
  variables = c("cyl", "am", "mpg", "hp"),
  type =
    list(
      cyl = "categorical",
      am = "dichotomous",
      mpg = "continuous",
      hp = "continuous2"
    ),
  statistic =
    list(
      cyl = "{n} / {N}",
      am = "{n} / {N}",
      mpg = "{mean} ({sd})",
      hp = c("{median} ({p25}, {p75})", "{mean} ({sd})")
    )
) |>
  as_tibble()
#> # A tibble: 9 × 2
#>   `**Characteristic**` stat_0             
#>   <chr>                <chr>              
#> 1 cyl                  NA                 
#> 2 4                    11 / 32            
#> 3 6                    7 / 32             
#> 4 8                    14 / 32            
#> 5 am                   13 / 32            
#> 6 mpg                  20.1 (6.0)         
#> 7 hp                   NA                 
#> 8 Median (Q1, Q3)      123.0 (96.0, 180.0)
#> 9 Mean (SD)            146.7 (68.6)       

pier_summary_dichotomous(
  cards = cards,
  variables = "am",
  statistic = list(am = "{n} ({p})")
)
#> # A tibble: 1 × 5
#>   row_type var_label variable label stat_0   
#>   <chr>    <chr>     <chr>    <chr> <chr>    
#> 1 label    am        am       am    13 (40.6)

pier_summary_categorical(
  cards = cards,
  variables = "cyl",
  statistic = list(cyl = "{n} ({p})")
)
#> # A tibble: 4 × 5
#>   variable var_label row_type label stat_0   
#>   <chr>    <chr>     <chr>    <chr> <chr>    
#> 1 cyl      cyl       label    cyl   NA       
#> 2 cyl      cyl       level    4     11 (34.4)
#> 3 cyl      cyl       level    6     7 (21.9) 
#> 4 cyl      cyl       level    8     14 (43.8)

pier_summary_continuous2(
  cards = cards,
  variables = "hp",
  statistic = list(hp = c("{median}", "{mean}"))
)
#> # A tibble: 3 × 5
#>   variable var_label row_type label  stat_0
#>   <chr>    <chr>     <chr>    <chr>  <chr> 
#> 1 hp       hp        label    hp     NA    
#> 2 hp       hp        level    Median 123.0 
#> 3 hp       hp        level    Mean   146.7 

pier_summary_continuous(
  cards = cards,
  variables = "mpg",
  statistic = list(mpg = "{median}")
)
#> # A tibble: 1 × 5
#>   row_type var_label variable label stat_0
#>   <chr>    <chr>     <chr>    <chr> <chr> 
#> 1 label    mpg       mpg      mpg   19.2  
```
