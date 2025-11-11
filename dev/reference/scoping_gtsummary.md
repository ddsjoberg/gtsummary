# Scoping for Table Body and Header

### `scope_table_body()`

This function uses the information in `.$table_body` and adds them as
attributes to `data` (if passed). Once they've been assigned as proper
gtsummary attributes, gtsummary selectors like
[`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)
will work properly.

Columns `c("var_type", "test_name", "contrasts_type")` and columns that
begin with `"selector_*"` are scoped. The values of these columns are
added as attributes to a data frame. For example, if
`var_type='continuous'` for variable `"age"`, then the attribute
`attr(.$age, 'gtsummary.var_type') <- 'continuous'` is set. That
attribute is then used in a selector like
[`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md).

### `scope_header()`

This function takes information from `.$table_styling$header` and adds
it to `table_body`. Columns that begin with `'modify_selector_'` and the
`hide` column.

## Usage

``` r
scope_table_body(table_body, data = NULL)

scope_header(table_body, header = NULL)
```

## Arguments

- table_body:

  a data frame from `.$table_body`

- data:

  an optional data frame the attributes will be added to

- header:

  the header data frame from `.$table_styling$header`

## Value

a data frame

## Examples

``` r
tbl <- tbl_summary(trial, include = c(age, grade))

scope_table_body(tbl$table_body) |> select(all_continuous()) |> names()
#> [1] "age"
```
