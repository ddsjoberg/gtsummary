# Remove rows

Removes either the header, reference, or missing rows from a gtsummary
table.

## Usage

``` r
remove_row_type(
  x,
  variables = everything(),
  type = c("header", "reference", "missing", "level", "all"),
  level_value = NULL
)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- variables:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to to remove rows from. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)

- type:

  (`string`)  
  Type of row to remove. Must be one of
  `c("header", "reference", "missing", "level", "all")`

- level_value:

  (`string`) When `type='level'` you can specify the *character* value
  of the level to remove. When `NULL` all levels are removed.

## Value

Modified gtsummary table

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  dplyr::mutate(
    age60 = ifelse(age < 60, "<60", "60+")
  ) |>
  tbl_summary(by = trt, missing = "no", include = c(trt, age, age60)) |>
  remove_row_type(age60, type = "header")


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

Age

46 (37, 60)

48 (39, 56)

    60+

23 (25%)

18 (18%)

    \<60

68 (75%)

80 (82%)

¹ Median (Q1, Q3); n (%)
