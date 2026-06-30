# Modify Abbreviations

All abbreviations will be coalesced when printing the final table into a
single specialized source note.

## Usage

``` r
modify_abbreviation(
  x,
  abbreviation,
  text_interpret = c("md", "html", "none"),
  prefix = NULL,
  sep1 = NULL,
  sep2 = NULL
)

remove_abbreviation(x, abbreviation = NULL)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- abbreviation:

  (`character`)  
  a character vector of one or more abbreviations to add. In
  `remove_abbreviation()`, the default value is `NULL`, which will
  remove all abbreviation source notes; otherwise a character vector of
  abbreviations to remove.

- text_interpret:

  (`string`)  
  String indicates whether text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default), `"html"`, or `"none"`. `"none"` applies no
  interpretation, rendering the text verbatim (useful when the text
  contains markdown-significant characters). Applies to tables printed
  with `{gt}`.

- prefix, sep1, sep2:

  (`character`)  
  optional arguments controlling how the abbreviation source note is
  assembled as
  `paste0(prefix, sep1, paste(abbreviations, collapse = sep2))`.

  - `prefix` is a character vector of length two giving the leading
    text, as `c(singular, plural)`. The first element is used when a
    single abbreviation is present and the second when multiple
    abbreviations are present. Pass `c("", "")` to omit the leading
    text. Defaults to `NULL`, in which case the theme element
    `"modify_abbreviation-arg:prefix"` is used, falling back to the
    translated default `c("Abbreviation", "Abbreviations")`. Unlike the
    default, a user-supplied `prefix` is used as-is and is not
    translated.

  - `sep1` is a string giving the separator between the prefix and the
    abbreviations. Defaults to `NULL`, in which case the theme element
    `"modify_abbreviation-arg:sep1"` is used, falling back to the
    default `": "`. It is omitted when the resolved prefix is empty.

  - `sep2` is a string giving the separator placed between
    abbreviations. Defaults to `NULL`, in which case the theme element
    `"modify_abbreviation-arg:sep2"` is used, falling back to the
    default `", "`.

## Value

Updated gtsummary object

## See also

[Footnotes vs Source Notes vs
Abbreviations](https://www.danieldsjoberg.com/gtsummary/articles/modify-functions.html)

## Examples

``` r
# Example 1 ----------------------------------
tbl_summary(
  trial,
  by = trt,
  include = age,
  type = age ~ "continuous2"
) |>
  modify_table_body(~dplyr::mutate(.x, label = sub("Q1, Q3", "IQR", x = label))) |>
  modify_abbreviation("IQR = Interquartile Range")


  

Characteristic
```
