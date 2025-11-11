# DEPRECATED Footnote

**\[deprecated\]**  
Use
[`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)
and
[`modify_abbreviation()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md)
instead.

## Usage

``` r
modify_footnote(
  x,
  ...,
  abbreviation = FALSE,
  text_interpret = c("md", "html"),
  update,
  quiet
)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- ...:

  [`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)  
  Used to assign updates to footnotes. Use
  `modify_footnote(colname='new footnote')` to update a single footnote.

- abbreviation:

  (scalar `logical`)  
  Logical indicating if an abbreviation is being updated.

- text_interpret:

  (`string`)  
  String indicates whether text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default) or `"html"`. Applies to tables printed with `{gt}`.

- update, quiet:

  **\[deprecated\]**

## Value

Updated gtsummary object

## Examples

``` r
# Use `modify_footnote_header()`, `modify_footnote_body()`, `modify_abbreviation()` instead.
```
