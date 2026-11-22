# Stratified Nested Stacking

This function stratifies your data frame, builds gtsummary tables, and
stacks the resulting tables in a nested style. The underlying
functionality is similar to
[`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_strata.md),
except the resulting tables are nested or indented within each group.

**NOTE**: The header from the first table is used for the final table.
Oftentimes, this header will include incorrect Ns and *must be updated.*

## Usage

``` r
tbl_strata_nested_stack(
  data,
  strata,
  .tbl_fun,
  ...,
  row_header = "{strata}",
  quiet = FALSE
)
```

## Arguments

- data:

  (`data.frame`)  
  a data frame

- strata:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  character vector or tidy-selector of columns in data to stratify
  results by. Only *observed* combinations are shown in results.

- .tbl_fun:

  (`function`) A function or formula. If a *function*, it is used as is.
  If a formula, e.g. `~ .x %>% tbl_summary() %>% add_p()`, it is
  converted to a function. The stratified data frame is passed to this
  function.

- ...:

  Additional arguments passed on to the `.tbl_fun` function.

- row_header:

  (`string`)  
  string indicating the row headers that appear in the table. The
  argument uses
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  syntax to insert values into the row headers. Elements available to
  insert are `strata`, `n`, `N` and `p`. The `strata` element is the
  variable level of the strata variables. Default is `'{strata}'`.

- quiet:

  (scalar `logical`)  
  Logical indicating whether to suppress additional messaging. Default
  is `FALSE`.

## Value

a stacked 'gtsummary' table

## Examples

``` r
# Example 1 ----------------------------------
tbl_strata_nested_stack(
  trial,
  strata = trt,
  .tbl_fun = ~ .x |>
    tbl_summary(include = c(age, grade), missing = "no") |>
    modify_header(all_stat_cols() ~ "**Summary Statistics**")
)


  

Characteristic
```
