# Save a gtsummary table to a Word file

**\[experimental\]**  
Save a gtsummary table or a flextable to a Word (`.docx`) file using the
flextable package.

**This function is highly experimental.** Its arguments and behavior are
likely to change in future releases, and it may eventually be spun off
into a separate package (as this function works with any flextable
object in addition to gtsummary tables). Use with that in mind.

The table is written into the **body** of the Word document. The `body`,
`header`, and `footer` arguments are transformers applied to the
(source) flextable to build, respectively, the content placed in the
document body and in the Word page **header** and **footer** regions
(which repeat on every page). Each is a function of a flextable, a
static `flextable`, or `NULL`.

By default the footnote region of the table (footnotes, source notes,
and abbreviations) is moved out of the body and into the Word footer as
a flextable, followed by a right-aligned `"Page X of Y"` line built from
live Word fields. Compose your own behavior with flextable functions
such as
[`flextable::delete_part()`](https://davidgohel.github.io/flextable/reference/delete_part.html),
[`flextable::add_footer_lines()`](https://davidgohel.github.io/flextable/reference/add_footer_lines.html),
and
[`flextable::as_word_field()`](https://davidgohel.github.io/flextable/reference/as_word_field.html).

A collection of tables is also accepted: a `tbl_split` object (from
[`tbl_split_by_rows()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_split_by.md)
or
[`tbl_split_by_columns()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_split_by.md)),
or a plain list of flextables. Each table is written to its own Word
section (one table per page) with the `body`/`header`/`footer`
transformers applied independently to each.

## Usage

``` r
save_flex_docx(
  x,
  path,
  body = function(x) {
     x %>% flextable::delete_part(part = "footer") %>%
    flextable::set_table_properties(layout = "autofit", width = 1)
 },
  footer = function(x) {
     x %>% flextable::delete_part(part = "header") %>%
    flextable::delete_part(part = "body") %>% flextable::add_footer_lines(values =
    flextable::as_paragraph("Page ", flextable::as_word_field("PAGE"), " of ",
    flextable::as_word_field("NUMPAGES"))) %>% flextable::align(i =
    flextable::nrow_part(x, "footer") + 1L, part = "footer", align = "right") %>%
    flextable::set_table_properties(layout = "autofit", width = 1)
 },
  header = NULL,
  template = NULL,
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

- body:

  (`function` or `NULL`)  
  a transformer applied to the source flextable to produce the flextable
  placed in the document body. The default removes the footnote region
  from the body (it is relocated to the Word footer by the `footer`
  default) and fits the table to 100% of the page width. `NULL` uses the
  source flextable unchanged.

- footer, header:

  (`function`, `flextable`, or `NULL`)  
  what to place in the Word page footer/header region: a transformer
  applied to the source flextable (returning a `flextable` or `NULL`), a
  static `flextable`, or `NULL` for nothing. The footer default keeps
  only the table's footnote region (deleting the header and body parts),
  appends a right-aligned `"Page X of Y"` line of live Word fields
  ([`flextable::as_word_field()`](https://davidgohel.github.io/flextable/reference/as_word_field.html)),
  and fits it to the page width. The header default is `NULL` (the
  caption stays in the body with the table).

- template:

  (`string`)  
  an optional file path to a Word (`.docx`) document used as the base
  for the output. Its page setup (size, orientation, margins) and body
  content are carried through; its header/footer text is not (those
  regions are managed by `save_flex_docx()`). See the *Using a Word
  template* section. Default is `NULL`.

- pr_section:

  ([`officer::prop_section`](https://davidgohel.github.io/officer/reference/prop_section.html))  
  an optional
  [`officer::prop_section()`](https://davidgohel.github.io/officer/reference/prop_section.html)
  object used as the base Word section, giving fine-grained control over
  page margins, page size, orientation, and section columns (e.g.
  `officer::prop_section(page_margins = officer::page_mar(top = 0.5))`).
  Only its geometry is used: `save_flex_docx()` always owns the
  header/footer regions, so any `header_default`/`footer_default` set on
  `pr_section` are ignored. For a collection (`tbl_split` or a list of
  flextables) the same geometry is applied to every table's section and
  the paging `type` is fixed to `"nextPage"` (any `type` on `pr_section`
  is ignored) so tables page correctly. Overrides the
  `save_flex_docx-lst:pr_section` theme element. Default is `NULL`.

- ...:

  These dots are for future extensions and must be empty.

## Value

the original object `x` (invisibly)

## Using a Word template

The `template` argument accepts a path to a Word (`.docx`) document used
as the base for the output. Its **page setup** (size, orientation,
margins, section columns) and any **body content** (e.g. a cover page or
introductory text) are carried through, with the table written into the
body after that content.

`save_flex_docx()` **manages the Word header and footer regions itself**
(via the `header`/`footer` arguments), so **a template's own
header/footer text is not carried through** — whatever
`save_flex_docx()` places in a region (or leaves empty) takes precedence
and blanks out the template's text there. This is intentional:
header/footer text in a template and table placement in the
header/footer are **not meant to be mixed**. Because the default
`footer` places a table, a template's header/footer text is superseded
by default. Put the content you want in the header/footer into the
`header`/`footer` arguments rather than into the template.

## See also

[`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/reference/as_flex_table.md)

## Examples

``` r
if (FALSE) { # FALSE && gtsummary:::is_pkg_installed(c("flextable", "officer"))
theme_gtsummary_compact()

# Example 1 ----------------------------------
# Default behavior is to place the footnote in the footer and add 'Page X of Y'
tbl <- tbl_summary(trial, by = trt, include = c(age, grade))

# by default the footnotes move to the Word footer with a page-number line
save_flex_docx(tbl, path = tempfile(fileext = ".docx"))

# Example 2 ----------------------------------
# This example places a header typically found in the pharmaceutical space,
# including protocol number, table title/number, and sub-population label.

# place a static report header (with a live "Page X of Y" field) in the header
header_ft <-
  data.frame(
    col1 = c("Protocol: ABC123", NA),
    col2 = c("Table 14.3.6 Adverse Event Rates by SOC and PT", "Safety Population"),
    col3 = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  ) |>
  flextable::flextable() |>
  flextable::delete_part(part = "header") |>
  flextable::align(j = 1, align = "left", part = "body") |>
  flextable::align(j = 2, align = "center", part = "body") |>
  flextable::align(j = 3, align = "right", part = "body") |>
  flextable::compose(
    i = 1, j = 3,
    value = flextable::as_paragraph(
      "Page ", flextable::as_word_field("PAGE"),
      " of ", flextable::as_word_field("NUMPAGES")
    ),
    part = "body"
  ) |>
  flextable::border_remove() |>
  flextable::fontsize(size = 8, part = "all") |>
  flextable::padding(padding.top = 0, padding.bottom = 0, part = "all") |>
  flextable::set_table_properties(layout = "autofit", width = 1)
cards::ADAE[1:150,] |>
  tbl_hierarchical(
    by = TRTA,
    variables = c(AESOC, AEDECOD),
    id = USUBJID,
    denominator = cards::ADSL
  ) |>
  save_flex_docx(path = tempfile(fileext = ".docx"), header = header_ft)

# Example 3 ----------------------------------
# a split table is written with one table per section/page, body footnotes
# only appear on the pages where they are represented
trial |>
  tbl_summary(by = trt, include = c(age, marker, grade), missing = ~"no") |>
  modify_footnote_body(footnote = "Age in years", columns = "label", rows = variable == "age") |>
  tbl_split_by_rows(variables = marker) |>
  save_flex_docx(path = tempfile(fileext = ".docx"))

# Example 4 ----------------------------------
# customize the Word page margins and orientation via a prop_section()
save_flex_docx(
  tbl,
  path = tempfile(fileext = ".docx"),
  pr_section = officer::prop_section(
    page_margins = officer::page_mar(top = 0.5, bottom = 0.5),
    page_size = officer::page_size(orient = "landscape")
  )
)

# Example 5 ----------------------------------
# keep the whole table (including footnotes) in the body, nothing in the footer
save_flex_docx(
  tbl,
  path = tempfile(fileext = ".docx"),
  body = NULL,
  footer = NULL
)

reset_gtsummary_theme()
}
```
