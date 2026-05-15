# Merge tables

Merge gtsummary tables, e.g. `tbl_regression`, `tbl_uvregression`,
`tbl_stack`, `tbl_summary`, `tbl_svysummary`, etc.

This function merges **like tables**. Generally, this means each of the
tables being merged should have the same structure. When merging tables
with different structures, rows may appear out of order. The ordering of
rows can be updated with `modify_table_body(~dplyr::arrange(.x, ...))`.

## Usage

``` r
tbl_merge(
  tbls,
  tab_spanner = NULL,
  merge_vars = NULL,
  tbl_ids = NULL,
  quiet = FALSE
)
```

## Arguments

- tbls:

  (`list`)  
  List of gtsummary objects to merge

- tab_spanner:

  (`character`)  
  Character vector specifying the spanning headers. Must be the same
  length as `tbls`. The strings are interpreted with
  [`gt::md`](https://gt.rstudio.com/reference/md.html). Must be same
  length as `tbls` argument. Default is `NULL`, and places a default
  spanning header. If `FALSE`, no header will be placed.

- merge_vars:

  (`character`)  
  Column names that are used as the merge IDs. The default is `NULL`,
  which merges on
  `c(any_of(c("variable", "row_type", "var_label", "label"), cards::all_ard_groups())`.
  Any column name included here that does not appear in all tables, will
  be removed.

- tbl_ids:

  (`character`)  
  Optional character vector of IDs that will be assigned to the input
  tables. The ID is assigned by assigning a name to the `tbls` list,
  which is returned in `x$tbls`.

- quiet:

  (scalar `logical`)  
  When `FALSE`, a message is printed when unlike tables are merged
  warning users of potential row ordering issues.

## Value

A `'tbl_merge'` object

## Author

Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
# Side-by-side Regression Models
library(survival)

t1 <-
  glm(response ~ trt + grade + age, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(exponentiate = TRUE)

tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)


  


Characteristic
```
