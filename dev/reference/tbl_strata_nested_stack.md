# Stratified Nested Stacking

This function stratifies your data frame, builds gtsummary tables, and
stacks the resulting tables in a nested style. The underlying
functionality is similar to
[`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md),
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

**Summary Statistics**¹

Drug A

  

    Age

46 (37, 60)

    Grade

  

        I

35 (36%)

        II

32 (33%)

        III

31 (32%)

Drug B

  

    Age

48 (39, 56)

    Grade

  

        I

33 (32%)

        II

36 (35%)

        III

33 (32%)

¹ Median (Q1, Q3); n (%)

\# Example 2 ---------------------------------- tbl_strata_nested_stack(
trial, strata = trt, .tbl_fun = ~ .x \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(include
= [c](https://rdrr.io/r/base/c.html)(age, grade), missing = "no") \|\>
[modify_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)([all_stat_cols](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "\*\*Summary Statistics\*\*"), row_header = "{strata}, n={n}" ) \|\>
\# bold the row headers; print \`x\$table_body\` to see hidden columns
[modify_bold](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md)(columns
= "label", rows = tbl_indent_id1 \> 0)

[TABLE]
