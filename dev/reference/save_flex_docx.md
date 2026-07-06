# Save a gtsummary table to a Word file

**\[experimental\]**  
Save a gtsummary table or a flextable to a Word (`.docx`) file using the
flextable package.

The `header` and `footer` arguments control where the table caption and
the footnote-region content (footnotes, source notes, and abbreviations)
are placed in the Word document. When `TRUE`, this content is moved to
the Word document's page **header** and **footer** regions (where it
repeats on every page) instead of appearing in the flow of the table
itself. When `FALSE`, the content is rendered as part of the table,
matching
[`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md).

A collection of tables is also accepted: a `tbl_split` object (from
[`tbl_split_by_rows()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_split_by.md)
or
[`tbl_split_by_columns()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_split_by.md)),
or a plain list of flextables. Each table is written to its own Word
section so that each table's caption and footnote-region content
populate that section's own header/footer regions, one table per page.

A `flextable` object (or a list of them) is also accepted. Its caption
([`flextable::set_caption()`](https://davidgohel.github.io/flextable/reference/set_caption.html))
is relocated to the Word header and its footer part
([`flextable::add_footer_lines()`](https://davidgohel.github.io/flextable/reference/add_footer_lines.html))
to the Word footer, matching the gtsummary behavior.

## Usage

``` r
save_flex_docx(
  x,
  path,
  header = FALSE,
  footer = FALSE,
  page = NULL,
  page_location = c("footer-right", "footer-center", "footer-left", "header-right",
    "header-center", "header-left"),
  pr_section = NULL,
  ...
)
```

## Arguments

- x:

  (`gtsummary`, `tbl_split`, `flextable`, or `list`)  
  a gtsummary table, a `tbl_split` object (a list of gtsummary tables),
  a `flextable` object, or a plain list of `flextable` objects

- path:

  (`string`)  
  file path to write the Word (`.docx`) file to

- header:

  (scalar `logical`)  
  whether to place the table caption in the Word document's page header
  region. When `FALSE`, the caption is rendered as the table caption.
  Default is `FALSE`.

- footer:

  (scalar `logical`)  
  whether to place the footnotes, source notes, and abbreviations in the
  Word document's page footer region. When `FALSE`, this content is
  rendered in the table's footer. Default is `FALSE`.

- page:

  (`string`)  
  an optional string added as a page-number line in a header/footer
  region. The tokens `{PAGE}` and `{NUMPAGES}` are replaced with live
  Word fields for the current page number and total page count (e.g.
  `"Page {PAGE} of {NUMPAGES}"`); all other text is added verbatim.
  Default is `NULL` (no page line). This is independent of the
  `header`/`footer` arguments.

- page_location:

  (`string`)  
  where to place the `page` text, as `"<region>-<alignment>"`. Must be
  one of `"footer-right"` (default), `"footer-center"`, `"footer-left"`,
  `"header-right"`, `"header-center"`, or `"header-left"`.

- pr_section:

  ([`officer::prop_section`](https://davidgohel.github.io/officer/reference/prop_section.html))  
  an optional
  [`officer::prop_section()`](https://davidgohel.github.io/officer/reference/prop_section.html)
  object used as the base Word section, giving fine-grained control over
  page margins, page size, orientation, and section columns (e.g.
  `officer::prop_section(page_margins = officer::page_mar(top = 0.5))`).
  The section's header and footer regions are always managed by
  `save_flex_docx()` (the relocated caption and notes), so any
  `header_default`/`footer_default` set on `pr_section` are ignored. For
  a collection (`tbl_split` or a list of flextables) the same
  `pr_section` is applied to every table's section, and the paging
  `type` is fixed to `"nextPage"` (any `type` on `pr_section` is
  ignored) so tables page correctly. Overrides the
  `save_flex_docx-lst:pr_section` theme element. Default is `NULL`.

- ...:

  These dots are for future extensions and must be empty.

## Value

the original object `x` (invisibly)

## Limitations

Relocating the caption to the Word header (`header = TRUE`) and the
footnotes, source notes, and abbreviations to the Word footer
(`footer = TRUE`) is lossy. Relocated content is rendered as plain text:
markdown and HTML are not interpreted and emphasis markers (`**bold**`,
`_italic_`) are stripped. Only the font family and size are carried
over; colors, indentation, per-cell styling, and column structure are
not preserved.

## See also

[`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)

## Examples

``` r
tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  modify_caption("**Table 1. Patient Characteristics**")

# save the table, placing caption in the header and notes in the footer
save_flex_docx(tbl, path = tempfile(fileext = ".docx"))

# add a "Page X of Y" line to the footer
save_flex_docx(
  tbl,
  path = tempfile(fileext = ".docx"),
  page = "Page {PAGE} of {NUMPAGES}"
)

# a split table is written with one table per section/page
trial |>
  tbl_summary(by = trt, include = c(age, marker, grade), missing = ~"no") |>
  modify_footnote_body(
    "Footnotes only appear on the pages where the mark is present",
    columns = "label",
    rows = label == "Age"
  ) |>
  tbl_split_by_rows(variables = marker) |>
  save_flex_docx(path = tempfile(fileext = ".docx"))

# a flextable (or a list of flextables) is also accepted
ft <-
  as_flex_table(tbl) |>
  flextable::set_caption("Table 1")
save_flex_docx(ft, path = tempfile(fileext = ".docx"))

# customize the Word page margins and orientation via a prop_section()
save_flex_docx(
  tbl,
  path = tempfile(fileext = ".docx"),
  pr_section = officer::prop_section(
    page_margins = officer::page_mar(top = 0.5, bottom = 0.5),
    page_size = officer::page_size(orient = "landscape")
  )
)
```
