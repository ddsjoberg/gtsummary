# Add model statistics

Add model statistics returned from
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html).
Statistics can either be appended to the table (`add_glance_table()`),
or added as a table source note (`add_glance_source_note()`).

## Usage

``` r
add_glance_table(
  x,
  include = everything(),
  label = NULL,
  fmt_fun = list(everything() ~ label_style_sigfig(digits = 3), any_of("p.value") ~
    label_style_pvalue(digits = 1), c(where(is.integer), starts_with("df")) ~
    label_style_number()),
  glance_fun = glance_fun_s3(x$inputs$x)
)

add_glance_source_note(
  x,
  include = everything(),
  label = NULL,
  fmt_fun = list(everything() ~ label_style_sigfig(digits = 3), any_of("p.value") ~
    label_style_pvalue(digits = 1), c(where(is.integer), starts_with("df")) ~
    label_style_number()),
  glance_fun = glance_fun_s3(x$inputs$x),
  text_interpret = c("md", "html"),
  sep1 = " = ",
  sep2 = "; "
)
```

## Arguments

- x:

  (`tbl_regression`)  
  a `'tbl_regression'` object

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  names of statistics to include in output. Must be column names of the
  tibble returned by
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  or from the `glance_fun` argument. The include argument can also be
  used to specify the order the statistics appear in the table.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  specifies statistic labels, e.g.
  `list(r.squared = "R2", p.value = "P")`

- fmt_fun:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the the functions used to format/round the glance
  statistics. The default is to round the number of observations and
  degrees of freedom to the nearest integer, p-values are styled with
  [`style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_pvalue.md)
  and the remaining statistics are styled with
  `style_sigfig(x, digits = 3)`

- glance_fun:

  (`function`)  
  function that returns model statistics. Default is `glance_fun()`
  (which is
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  for most model objects). Custom functions must return a single row
  tibble.

- text_interpret:

  (`string`)  
  String indicates whether source note text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default) or `"html"`.

- sep1:

  (`string`)  
  Separator between statistic name and statistic. Default is `" = "`,
  e.g. `"R2 = 0.456"`

- sep2:

  (`string`)  
  Separator between statistics. Default is `"; "`

## Value

gtsummary table

## Tips

When combining `add_glance_table()` with
[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md),
the ordering of the model terms and the glance statistics may become
jumbled. To re-order the rows with glance statistics on bottom, use the
script below:

    tbl_merge(list(tbl1, tbl2)) |>
      modify_table_body(~.x |> dplyr::arrange(row_type == "glance_statistic"))

## Examples

``` r
mod <- lm(age ~ marker + grade, trial) |> tbl_regression()

# Example 1 ----------------------------------
mod |>
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )


  

Characteristic
```

**Beta**

**95% CI**

**p-value**

Marker Level (ng/mL)

-0.04

-2.6, 2.5

\>0.9

Grade

  

  

  

    I

—

—

  

    II

0.64

-4.7, 6.0

0.8

    III

2.4

-2.8, 7.6

0.4

R²

0.005

  

  

AIC

1,473

  

  

σ

14.6

  

  

Abbreviation: CI = Confidence Interval

\# Example 2 ---------------------------------- mod \|\>
add_glance_source_note( label =
[list](https://rdrr.io/r/base/list.html)(sigma = "\U03C3"), include =
[c](https://rdrr.io/r/base/c.html)(r.squared, AIC, sigma) )

[TABLE]
