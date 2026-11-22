# Convert gtsummary object to a kableExtra object

Function converts a gtsummary object to a knitr_kable + kableExtra
object. This allows the customized formatting available via
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) and
{kableExtra}; `as_kable_extra()` supports arguments in
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).
`as_kable_extra()` output via gtsummary supports bold and italic cells
for table bodies. Users are encouraged to leverage `as_kable_extra()`
for enhanced pdf printing; for html output options there is better
support via
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md).

## Usage

``` r
as_kable_extra(
  x,
  escape = FALSE,
  format = NULL,
  ...,
  include = everything(),
  addtl_fmt = TRUE,
  return_calls = FALSE
)
```

## Arguments

- x:

  (`gtsummary`)  
  Object created by a function from the gtsummary package (e.g.
  [tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  or
  [tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md))

- format, escape, ...:

  arguments passed to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html). Default
  is `escape = FALSE`, and the format is auto-detected.

- include:

  Commands to include in output. Input may be a vector of quoted or
  unquoted names. tidyselect and gtsummary select helper functions are
  also accepted. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- addtl_fmt:

  logical indicating whether to include additional formatting. Default
  is `TRUE`. This is primarily used to escape special characters,
  convert markdown to LaTeX, and remove line breaks from the footnote.

- return_calls:

  Logical. Default is `FALSE`. If `TRUE`, the calls are returned as a
  list of expressions.

## Value

A {kableExtra} table

## PDF/LaTeX

This section shows options intended for use with `output: pdf_document`
in yaml of `.Rmd`.

When the default values of
`as_kable_extra(escape = FALSE, addtl_fmt = TRUE)` are utilized, the
following formatting occurs.

- Markdown bold, italic, and underline syntax in the headers, spanning
  headers, caption, and footnote will be converted to escaped LaTeX code

- Special characters in the table body, headers, spanning headers,
  caption, and footnote will be escaped with
  [`.escape_latex()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/kableExtra_utils.md)
  or
  [`.escape_latex2()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/kableExtra_utils.md)

- The `"\n"` symbol will be recognized as a line break in the table
  headers, spanning headers, caption, and the table body

- The `"\n"` symbol is removed from the footnotes

To suppress *these* additional formats, set
`as_kable_extra(addtl_fmt = FALSE)`

Additional styling is available with
[`kableExtra::kable_styling()`](https://rdrr.io/pkg/kableExtra/man/kable_styling.html)
as shown in Example 2, which implements row striping and repeated column
headers in the presence of page breaks.

## HTML

This section discusses options intended for use with
`output: html_document` in yaml of `.Rmd`.

When the default values of
`as_kable_extra(escape = FALSE, addtl_fmt = TRUE)` are utilized, the
following formatting occurs.

- The default markdown syntax in the headers and spanning headers is
  removed

- Special characters in the table body, headers, spanning headers,
  caption, and footnote will be escaped with
  [`.escape_html()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/kableExtra_utils.md)

- The `"\n"` symbol is removed from the footnotes

To suppress the additional formatting, set
`as_kable_extra(addtl_fmt = FALSE)`

## Author

Daniel D. Sjoberg

## Examples

``` r
# basic gtsummary tbl to build upon
as_kable_extra_base <-
  trial |>
  tbl_summary(by = trt, include = c(age, stage)) |>
  bold_labels()

# Example 1 (PDF via LaTeX) ---------------------
# add linebreak in table header with '\n'
as_kable_extra_ex1_pdf <-
  as_kable_extra_base |>
  modify_header(all_stat_cols() ~ "**{level}**  \n*N = {n}*") |>
  as_kable_extra()

# Example 2 (PDF via LaTeX) ---------------------
# additional styling in `knitr::kable()` and with
#   call to `kableExtra::kable_styling()`
as_kable_extra_ex2_pdf <-
  as_kable_extra_base |>
  as_kable_extra(
    booktabs = TRUE,
    longtable = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_styling(
    position = "left",
    latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15"
  )
```
