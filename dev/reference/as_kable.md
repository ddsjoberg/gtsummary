# Convert gtsummary object to a kable object

Output from [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
is less full featured compared to summary tables produced with
[gt](https://gt.rstudio.com/index.html). For example, kable summary
tables do not include indentation, footnotes, or spanning header rows.

Line breaks (`\n`) are removed from column headers and table cells.

## Usage

``` r
as_kable(x, ..., include = everything(), return_calls = FALSE)
```

## Arguments

- x:

  (`gtsummary`)  
  Object created by a function from the gtsummary package (e.g.
  [tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  or
  [tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md))

- ...:

  Additional arguments passed to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)

- include:

  Commands to include in output. Input may be a vector of quoted or
  unquoted names. tidyselect and gtsummary select helper functions are
  also accepted. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- return_calls:

  Logical. Default is `FALSE`. If `TRUE`, the calls are returned as a
  list of expressions.

## Value

A `knitr_kable` object

## Details

Tip: To better distinguish variable labels and level labels when
indenting is not supported, try
[`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)
or
[`italicize_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md).

## Author

Daniel D. Sjoberg

## Examples

``` r
trial |>
  tbl_summary(by = trt) |>
  bold_labels() |>
  as_kable()
#> 
#> 
#> |**Characteristic**         | **Drug A**  N = 98 | **Drug B**  N = 102 |
#> |:--------------------------|:------------------:|:-------------------:|
#> |__Age__                    |    46 (37, 60)     |     48 (39, 56)     |
#> |Unknown                    |         7          |          4          |
#> |__Marker Level (ng/mL)__   | 0.84 (0.23, 1.60)  |  0.52 (0.18, 1.21)  |
#> |Unknown                    |         6          |          4          |
#> |__T Stage__                |                    |                     |
#> |T1                         |      28 (29%)      |      25 (25%)       |
#> |T2                         |      25 (26%)      |      29 (28%)       |
#> |T3                         |      22 (22%)      |      21 (21%)       |
#> |T4                         |      23 (23%)      |      27 (26%)       |
#> |__Grade__                  |                    |                     |
#> |I                          |      35 (36%)      |      33 (32%)       |
#> |II                         |      32 (33%)      |      36 (35%)       |
#> |III                        |      31 (32%)      |      33 (32%)       |
#> |__Tumor Response__         |      28 (29%)      |      33 (34%)       |
#> |Unknown                    |         3          |          4          |
#> |__Patient Died__           |      52 (53%)      |      60 (59%)       |
#> |__Months to Death/Censor__ | 23.5 (17.4, 24.0)  |  21.2 (14.5, 24.0)  |
```
