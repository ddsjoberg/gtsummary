# Special Character Escape

These utility functions were copied from the internals of kableExtra,
and assist in escaping special characters in LaTeX and HTML tables.
These function assist in the creations of tables via
[`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md).

## Usage

``` r
.escape_html(x)

.escape_latex(x, newlines = TRUE, align = "c")

.escape_latex2(x, newlines = TRUE, align = "c")
```

## Arguments

- x:

  character vector

## Value

character vector with escaped special characters

## See also

[`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)

## Examples

``` r
.escape_latex(c("%", "{test}"))
#> [1] "\\%"        "\\{test\\}"
.escape_html(c(">0.9", "line\nbreak"))
#> [1] "&gt;0.9"         "line<br />break"
```
