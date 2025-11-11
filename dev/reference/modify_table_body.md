# Modify Table Body

Function is for advanced manipulation of gtsummary tables. It allow
users to modify the `.$table_body` data frame included in each gtsummary
object.

If a new column is added to the table, default printing instructions
will then be added to `.$table_styling`. By default, columns are hidden.
To show a column, add a column header with
[`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
or call
[`modify_column_unhide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md).

## Usage

``` r
modify_table_body(x, fun, ...)
```

## Arguments

- x:

  (`gtsummary`)  
  A 'gtsummary' object

- fun:

  (`function`)  
  A function or formula. If a *function*, it is used as is. If a
  *formula*, e.g. `fun = ~ .x |> arrange(variable)`, it is converted to
  a function. The argument passed to `fun` is `x$table_body`.

- ...:

  Additional arguments passed on to the function

## Value

A 'gtsummary' object

## Examples

``` r
# Example 1 --------------------------------
# Add number of cases and controls to regression table
trial |>
 tbl_uvregression(
   y = response,
   include = c(age, marker),
   method = glm,
   method.args = list(family = binomial),
   exponentiate = TRUE,
   hide_n = TRUE
 ) |>
 # adding number of non-events to table
 modify_table_body(
   ~ .x %>%
     dplyr::mutate(N_nonevent = N_obs - N_event) |>
     dplyr::relocate(c(N_event, N_nonevent), .before = estimate)
 ) |>
 # assigning header labels
 modify_header(N_nonevent = "**Control N**", N_event = "**Case N**") |>
 modify_fmt_fun(c(N_event, N_nonevent) ~ style_number)


  

Characteristic
```

**Case N**

**Control N**

**OR**

**95% CI**

**p-value**

Age

58

125

1.02

1.00, 1.04

0.10

Marker Level (ng/mL)

57

126

1.35

0.94, 1.93

0.10

Abbreviations: CI = Confidence Interval, OR = Odds Ratio
