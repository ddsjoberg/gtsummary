# Modify Footnote Symbols

- `modify_footnote_symbol()` customizes the symbols used to reference
  footnotes in a gtsummary table. By default, footnotes are referenced
  with sequential numbers (`1, 2, 3, ...`). This function replaces those
  numbers with a user-specified set of symbols, e.g.
  `c("*", "\u2020", "\u2021")` (an asterisk, dagger, and double dagger).

- `remove_footnote_symbol()` resets the footnote reference marks back to
  the default numbering.

## Usage

``` r
modify_footnote_symbol(x, symbol)

remove_footnote_symbol(x)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- symbol:

  (`character`)  
  a character vector of length 2 or greater giving the ordered symbols
  used to reference footnotes, e.g. `c("*", "\u2020", "\u2021")`.
  Symbols are assigned to footnotes in the order the footnotes appear in
  the table. When the number of footnotes exceeds the number of symbols
  supplied, the symbols are recycled. A length of at least 2 is required
  because
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
  passes these to
  [`gt::opt_footnote_marks()`](https://gt.rstudio.com/reference/opt_footnote_marks.html),
  which interprets a single string as the name of a built-in mark
  scheme. Currently only utilized by
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
  and
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md).

## Value

Updated gtsummary object

## Details

The symbol sequence may also be set for all tables via the
`"pkgwide-chr:footnote_symbol"` theme element; a value set with
`modify_footnote_symbol()` takes precedence over the theme element.

## Common footnote marks

The table below lists common footnote reference marks and the R strings
used to create them. The six marks correspond to those used in
`gt::opt_footnote_marks(marks = "extended")`. Pass the R string (e.g.
`"\u2020"`) to the `symbol` argument to obtain the corresponding mark.

|                          |          |            |
|--------------------------|----------|------------|
| Name                     | Unicode  | R string   |
| asterisk                 | `U+002A` | `"*"`      |
| dagger                   | `U+2020` | `"\u2020"` |
| double dagger            | `U+2021` | `"\u2021"` |
| section sign             | `U+00A7` | `"\u00A7"` |
| double vertical line     | `U+2016` | `"\u2016"` |
| pilcrow (paragraph sign) | `U+00B6` | `"\u00B6"` |

## See also

[`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
[`modify_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
[`modify_footnote_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)

## Examples

``` r
# use symbols (instead of numbers) to reference footnotes
trial |>
  tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
  add_p() |>
  modify_footnote_symbol(symbol = c("*", "\u2020", "\u2021"))


  

Characteristic
```
