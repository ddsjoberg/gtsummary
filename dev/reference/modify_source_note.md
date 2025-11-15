# Modify source note

Add and remove source notes from a table. Source notes are similar to
footnotes, expect they are not linked to a cell in the table.

## Usage

``` r
modify_source_note(x, source_note, text_interpret = c("md", "html"))

remove_source_note(x, source_note_id = NULL)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object.

- source_note:

  (`string`)  
  A string to add as a source note.

- text_interpret:

  (`string`)  
  String indicates whether text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default) or `"html"`. Applies to tables printed with `{gt}`.

- source_note_id:

  (`integers`)  
  Integers specifying the IDs of the source notes to remove. Source
  notes are indexed sequentially at the time of creation. Default is
  `NULL`, which removes all source notes.

## Value

gtsummary object

## Details

Source notes are not supported by
[`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md).

## Examples

``` r
# Example 1 ----------------------------------
tbl <- tbl_summary(trial, include = c(marker, grade), missing = "no") |>
  modify_source_note("Results as of June 26, 2015")
tbl


  

Characteristic
```

**N = 200**¹

Marker Level (ng/mL)

0.64 (0.22, 1.41)

Grade

  

    I

68 (34%)

    II

68 (34%)

    III

64 (32%)

¹ Median (Q1, Q3); n (%)

Results as of June 26, 2015

\# Example 2 ---------------------------------- remove_source_note(tbl,
source_note_id = 1)

[TABLE]
