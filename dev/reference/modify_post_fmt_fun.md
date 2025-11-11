# Modify post formatting

**\[experimental\]**  
Apply a formatting function after the primary formatting functions have
been applied. The function is similar to
[`gt::text_transform()`](https://gt.rstudio.com/reference/text_transform.html).

## Usage

``` r
modify_post_fmt_fun(x, fmt_fun, columns, rows = TRUE)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- fmt_fun:

  (`function`)  
  a function that will be applied to the specified columns and rows.

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Selector of columns in `x$table_body`

- rows:

  (predicate `expression`)  
  Predicate expression to select rows in `x$table_body`. Review [rows
  argument
  details](https://www.danieldsjoberg.com/gtsummary/dev/reference/rows_argument.md).

## Value

Updated gtsummary object

## Examples

``` r
# Example 1 ----------------------------------
data.frame(x = FALSE) |>
  tbl_summary(type = x ~ "categorical") |>
  modify_post_fmt_fun(
    fmt_fun = ~ifelse(. == "0 (0%)", "0", .),
    columns = all_stat_cols()
  )


  

Characteristic
```

**N = 1**¹

x

  

    FALSE

1 (100%)

    TRUE

0

¹ n (%)
