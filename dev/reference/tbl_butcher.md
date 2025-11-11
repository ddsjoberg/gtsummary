# Butcher table

Some gtsummary objects can become large and the size becomes cumbersome
when working with the object. The function removes all elements from a
gtsummary object, except those required to print the table. This may
result in gtsummary functions that add information or modify the table,
such as
[`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md),
will no longer execute after the excess elements have been removed (aka
butchered). Of note, the majority of
[`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md)
calls will continue to execute properly.

## Usage

``` r
tbl_butcher(x, include = c("table_body", "table_styling"))
```

## Arguments

- x:

  (`gtsummary`)  
  a gtsummary object

- include:

  (`character`)  
  names of additional elements to retain in the gtsummary object.
  `c("table_body", "table_styling")` will always be retained.

## Value

a gtsummary object

## Examples

``` r
tbl_large <-
  trial |>
  tbl_uvregression(
    y = age,
    method = lm
  )

tbl_butchered <-
  tbl_large |>
  tbl_butcher()

# size comparison
object.size(tbl_large) |> format(units = "Mb")
#> [1] "8.2 Mb"
object.size(tbl_butchered)|> format(units = "Mb")
#> [1] "1.5 Mb"
```
