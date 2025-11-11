# Modify column headers and spanning headers

These functions assist with modifying the aesthetics/style of a table.

- `modify_header()` update column headers

- `modify_spanning_header()` update/add spanning headers

The functions often require users to know the underlying column names.
Run `show_header_names()` to print the column names to the console.

## Usage

``` r
modify_header(x, ..., text_interpret = c("md", "html"), quiet, update)

modify_spanning_header(
  x,
  ...,
  text_interpret = c("md", "html"),
  level = 1L,
  quiet,
  update
)

remove_spanning_header(x, columns = everything(), level = 1L)

show_header_names(x, show_hidden = FALSE, include_example, quiet)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- ...:

  [`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)  
  Used to assign updates to headers and spanning headers.

  Use `modify_*(colname='new header')` to update a single column. Using
  a formula will invoke tidyselect, e.g.
  `modify_*(all_stat_cols() ~ "**{level}**")`. The dynamic dots allow
  syntax like `modify_header(x, !!!list(label = "Variable"))`. See
  examples below.

  Use the `show_header_names()` to see the column names that can be
  modified.

- text_interpret:

  (`string`)  
  String indicates whether text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default) or `"html"`. Applies to tables printed with `{gt}`.

- update, quiet:

  **\[deprecated\]**

- level:

  (`integer`)  
  An integer specifying which level to place the spanning header.

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Columns from which to remove spanning headers.

- show_hidden:

  (scalar `logical`)  
  Logical indicating whether to print hidden columns as well as printed
  columns. Default is `FALSE`.

- include_example:

  **\[deprecated\]**

## Value

Updated gtsummary object

## [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md), [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md), and [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)

When assigning column headers and spanning headers, you may use `{N}` to
insert the number of observations. `tbl_svysummary` objects additionally
have `{N_unweighted}` available.

When there is a stratifying `by=` argument present, the following fields
are additionally available to stratifying columns: `{level}`, `{n}`, and
`{p}` (`{n_unweighted}` and `{p_unweighted}` for `tbl_svysummary`
objects)

Syntax follows
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), e.g.
`all_stat_cols() ~ "**{level}**, N = {n}"`.

## tbl_regression()

When assigning column headers for `tbl_regression` tables, you may use
`{N}` to insert the number of observations, and `{N_event}` for the
number of events (when applicable).

## Author

Daniel D. Sjoberg

## Examples

``` r
# create summary table
tbl <- trial |>
  tbl_summary(by = trt, missing = "no", include = c("age", "grade", "trt")) |>
  add_p()

# print the column names that can be modified
show_header_names(tbl)
#> Column Name   Header                    level*         N*          n*          p*             
#> label         "**Characteristic**"                     200 <int>                              
#> stat_1        "**Drug A**  \nN = 98"    Drug A <chr>   200 <int>    98 <int>   0.490 <dbl>    
#> stat_2        "**Drug B**  \nN = 102"   Drug B <chr>   200 <int>   102 <int>   0.510 <dbl>    
#> p.value       "**p-value**"                            200 <int>                              
#> 
#> * These values may be dynamically placed into headers (and other locations).
#> ℹ Review the `modify_header()` (`?gtsummary::modify_header()`) help for
#>   examples.

# Example 1 ----------------------------------
# updating column headers
tbl |>
  modify_header(label = "**Variable**", p.value = "**P**")


  

Variable
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**P**²

Age

46 (37, 60)

48 (39, 56)

0.7

Grade

  

  

0.9

    I

35 (36%)

33 (32%)

  

    II

32 (33%)

36 (35%)

  

    III

31 (32%)

33 (32%)

  

¹ Median (Q1, Q3); n (%)

² Wilcoxon rank sum test; Pearson’s Chi-squared test

\# Example 2 ---------------------------------- \# updating headers add
spanning header tbl \|\>
modify_header([all_stat_cols](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "\*\*{level}\*\*, N = {n} ({style_percent(p)}%)") \|\>
modify_spanning_header([all_stat_cols](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)()
~ "\*\*Treatment Received\*\*")

[TABLE]
