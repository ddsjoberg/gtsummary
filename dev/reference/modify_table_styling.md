# Modify Table Styling

**This function is for developers.** *This function has very little
checking of the passed arguments, by design.*

If you are not a developer, it's recommended that you use the following
functions to make modifications to your table:

[`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md),
[`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md),
[`modify_column_hide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md),
[`modify_column_unhide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md),
[`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
[`modify_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
[`modify_abbreviation()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md),
[`modify_column_alignment()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_alignment.md),
[`modify_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_fmt_fun.md),
[`modify_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_indent.md),
[`modify_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md),
[`modify_missing_symbol()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_missing_symbol.md),
[`modify_bold()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md),
[`modify_italic()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md).

This is a function provides control over the characteristics of the
resulting gtsummary table by directly modifying `.$table_styling`.

Review the [gtsummary
definition](https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html)
vignette for information on `.$table_styling` objects.

## Usage

``` r
modify_table_styling(
  x,
  columns,
  rows = NULL,
  label = NULL,
  spanning_header = NULL,
  hide = NULL,
  footnote = NULL,
  footnote_abbrev = NULL,
  align = NULL,
  missing_symbol = NULL,
  fmt_fun = NULL,
  text_format = NULL,
  undo_text_format = NULL,
  indent = NULL,
  text_interpret = "md",
  cols_merge_pattern = NULL
)
```

## Arguments

- x:

  (`gtsummary`)  
  gtsummary object

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Selector of columns in `x$table_body`

- rows:

  (predicate `expression`)  
  Predicate expression to select rows in `x$table_body`. Can be used to
  style footnote, formatting functions, missing symbols, and text
  formatting. Default is `NULL`. See details below.

- label:

  (`character`)  
  Character vector of column label(s). Must be the same length as
  `columns`.

- spanning_header:

  (`string`)  
  string with text for spanning header

- hide:

  (scalar `logical`)  
  Logical indicating whether to hide column from output

- footnote:

  (`string`)  
  string with text for footnote

- footnote_abbrev:

  (`string`)  
  string with abbreviation definition, e.g. `"CI = Confidence Interval"`

- align:

  (`string`)  
  String indicating alignment of column, must be one of
  `c("left", "right", "center")`

- missing_symbol:

  (`string`)  
  string indicating how missing values are formatted.

- fmt_fun:

  (`function`)  
  function that formats the statistics in the columns/rows in `columns`
  and `rows`

- text_format, undo_text_format:

  (`string`)  
  String indicated which type of text formatting to apply/remove to the
  rows and columns. Must be one of `c("bold", "italic")`.

- indent:

  (`integer`)  
  An integer indicating how many space to indent text

- text_interpret:

  (`string`)  
  Must be one of `"md"` or `"html"` and indicates the processing
  function as [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Use this
  in conjunction with arguments for header and footnotes.

- cols_merge_pattern:

  (`string`)  
  glue-syntax string indicating how to merge columns in `x$table_body`.
  For example, to construct a confidence interval use
  `"{conf.low}, {conf.high}"`. The first column listed in the pattern
  string must match the single column name passed in `columns=`.

## rows argument

The rows argument accepts a predicate expression that is used to specify
rows to apply formatting. The expression must evaluate to a logical when
evaluated in `x$table_body`. For example, to apply formatting to the age
rows pass `rows = variable == "age"`. A vector of row numbers is NOT
acceptable.

A couple of things to note when using the `rows` argument.

1.  You can use saved objects to create the predicate argument, e.g.
    `rows = variable == letters[1]`.

2.  The saved object cannot share a name with a column in
    `x$table_body`. The reason for this is that in
    [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
    the columns are renamed, and the renaming process cannot
    disambiguate the `variable` column from an external object named
    `variable` in the following expression
    `rows = .data$variable = .env$variable`.

## cols_merge_pattern argument

There are planned updates to the implementation of column merging.
Currently, this function replaces the numeric column with a formatted
character column following `cols_merge_pattern=`. Once
[`gt::cols_merge()`](https://gt.rstudio.com/reference/cols_merge.html)
gains the `rows=` argument the implementation will be updated to use it,
which will keep numeric columns numeric. For the *vast majority* of
users, *the planned change will be go unnoticed*.

If this functionality is used in conjunction with
[`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
(which includes
[`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)),
there is potential issue with printing. When columns are stack AND when
the column-merging is defined with a quosure, you may run into issues
due to the loss of the environment when 2 or more quosures are combined.
If the expression version of the quosure is the same as the quosure
(i.e. no evaluated objects), there should be no issues. Regardless, this
argument is used internally with care, and it is *not* recommended for
users.

## See also

See [gtsummary internals
vignette](https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html)

Other Advanced modifiers:
[`modify_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md),
[`modify_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_indent.md)
