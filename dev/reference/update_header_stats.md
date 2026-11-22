# Update Header Statistics

**\[experimental\]**  
Use this function to update the statistics that can be injected into
gtsummary table headers with
[`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md),
e.g. `{N}`, `{n}`, `{p}`, and `{level}`. This is useful, for example,
when the headers should report population Ns that differ from the Ns in
the data summarized in the table.

- **Tables with a `by` variable** (columns `'stat_1'`, `'stat_2'`,
  etc.): pass a univariate tabulation of the `by` variable, e.g.
  `cards::ard_tabulate(variables = trt)`. The levels of the tabulated
  variable are matched *by position* to the `'stat_1'`, `'stat_2'`, etc.
  columns. If the table also includes an overall `'stat_0'` column (e.g.
  from
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)),
  its statistics are updated with the total N from the tabulation.

- **Tables without a `by` variable** (a single `'stat_0'` column): pass
  the total N, e.g.
  [`cards::ard_total_n()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_total_n.html).

Statistics are glued into the headers when
[`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
is called. Therefore, after updating the statistics, you must call
[`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
to see the updated numbers in the table.

## Usage

``` r
update_header_stats(x, card)
```

## Arguments

- x:

  (`gtsummary`)  
  Object with class 'gtsummary'

- card:

  (`card`)  
  Object of class 'card'. Either a univariate tabulation, typically
  created with
  [`cards::ard_tabulate()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_tabulate.html),
  or the total N, typically created with
  [`cards::ard_total_n()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_total_n.html).
  See details above.

## Value

gtsummary object

## Examples

``` r
# Example 1 ----------------------------------
tbl_summary(trial, by = trt, include = age) |>
  update_header_stats(
    # doubling the trial data set and the Ns
    card =
      rbind(trial, trial) |>
      cards::ard_tabulate(variables = trt)
  ) |>
  modify_header(all_stat_cols() ~ "**{level}**  \n N = {n}")
#> ! You must update the headers to utilize the updated numbers.
#> ℹ For example, `modify_header(all_stat_cols() ~ '**{level}**, N = {n}')`


  

Characteristic
```
