# Modifier Functions

``` r
library(gtsummary)
theme_gtsummary_compact()
#> Setting theme "Compact"
```

## Introduction

Oftentimes when a table is created using {gtsummary} the default
formatting does not exactly match a user’s preferred formatting.

To make modifications to tables after the table has been built, the
`modify_*()` functions were created.

In the following sections we briefly outline each of these functions
that can be used to modify gtsummary tables.

Many of the modifier functions share the common `columns` argument. When
this argument is required, the column names provided should match their
names in the table’s `table_body` element (i.e. `x$table_body`).

These column names can be printed to the console by running
`show_header_names(x)`.

When the `rows` argument is available for a modifier function, a subset
of rows in the table on which the function should operate can also be
specified via a predicate expression selecting rows from `x$table_body`
(see the function documentation for details).

We will begin by crafting a basic table to summarize a regression model.

In the following sections, we will showcase how to use a variety of
table modifier functions by applying them to this basic table.

``` r
x <- trial |>
  tbl_summary(
    by = trt,
    include = c(age, grade, response, death)
  ) |> 
  add_difference()

x
```

[TABLE]

Using
[`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
we can see that the following variables are present in `x$table_body`
and can be used when specifying the `columns` and `rows` arguments
common to many of the `modify_*()` functions.

The variables with a † next to their column names are currently hidden
from the table, but can be unhidden using the
[`modify_column_unhide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md)
function.

Hidden columns such as `variable`, `var_type`, `row_type`, etc. are
typically not shown in a table but are often used to specify a `rows`
predicate expression, as demonstrated in several of the following
examples.

``` r
show_header_names(x, show_hidden = TRUE)
#> Column Name   Header                    level*         N*          n*          p*             
#> variable†     "variable"                               200 <int>                              
#> test_name†    "test_name"                              200 <int>                              
#> var_type†     "var_type"                               200 <int>                              
#> row_type†     "row_type"                               200 <int>                              
#> var_label†    "var_label"                              200 <int>                              
#> label         "**Characteristic**"                     200 <int>                              
#> stat_1        "**Drug A**  \nN = 98"    Drug A <chr>   200 <int>    98 <int>   0.490 <dbl>    
#> stat_2        "**Drug B**  \nN = 102"   Drug B <chr>   200 <int>   102 <int>   0.510 <dbl>    
#> estimate      "**Difference**"                         200 <int>                              
#> std.error†    "**SE**"                                 200 <int>                              
#> parameter†    "**Parameter**"                          200 <int>                              
#> statistic†    "**Statistic**"                          200 <int>                              
#> ci†           "**95% CI**"                             200 <int>                              
#> conf.low      "**95% CI**"                             200 <int>                              
#> conf.high†    "conf.high"                              200 <int>                              
#> p.value       "**p-value**"                            200 <int>                             
#> * These values may be dynamically placed into headers (and other locations).
#> ℹ Review the `modify_header()` (`?gtsummary::modify_header()`) help for
#>   examples.
#> † Hidden columns
```

## Column Formatting Modifiers

- [`modify_column_alignment()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_alignment.md):
  Update column alignment/justification
- [`modify_column_hide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md)/[`modify_column_unhide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md):
  Hide/unhide table columns

These functions are used to apply column-wise formatting updates to a
table.

For each of these functions the column(s) on which to operate must be
specified via the `columns` argument.

``` r
x |>
  # unhide the standard error column
  modify_column_unhide(columns = std.error) |>
  # align the p-value column to the right
  modify_column_alignment(columns = p.value, align = "right")
```

[TABLE]

## Column Merging Modifier

- [`modify_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md)/[`remove_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md):
  Merge/un-merge two or more columns

These functions are used to merge/un-merge columns within a table and
should be used with caution.

When merging columns, the `pattern` argument is used to specify the glue
syntax that should be used to construct the merged column, and the
`rows` argument can be specified if merging should only take place in
specific rows.

The merged results will be displayed in the first column specified in
the `pattern` string - for example
`pattern = "{estimate} ({std.error})"` would display the merged result
in the `estimate` column.

Note that confidence interval columns are, by default, the result of
merging two columns - `conf.low` and `conf.high` - into the `conf.low`
column.

The
[`remove_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md)
function will undo column merging that was previously specified via
[`modify_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md).

``` r
x |>
  # merge the estimate and p.value columns in rows with non-NA p.value
  modify_column_merge(pattern = "{estimate} (p = {p.value})", rows = !is.na(p.value))
```

[TABLE]

## Indentation Modifier

- [`modify_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_indent.md):
  Update cell indentation

This function is used to increase/decrease indentation for table cells.

Indentation modifications can be applied across entire columns or rows,
or to specific cells.

The `columns` argument must always be specified.

In addition to specifying the column(s) to modify, the
[`modify_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_indent.md)
function also optionally accepts a `rows` argument if indentation should
only be changed on a particular subset of rows.

By default, `rows = NULL` so that indentation is applied across the
entire column.

To apply indentation across an entire row (or multiple rows) set
`columns = everything()` and `rows` to the specific rows that should be
modified.

If both `columns` and `rows` are specified (and not
[`everything()`](https://tidyselect.r-lib.org/reference/everything.html)),
indentation will be applied to specific cells in the table.

``` r
x |>
  # remove indentation across the label column
  modify_indent(columns = label, indent = 0L) |>
  # increase indentation to 10 for cells in stat_1 column with value of "3"
  modify_indent(columns = stat_1, rows = stat_1 == "3", indent = 10L) |>
  # increase indentation to 5 across the conf.low column in dichotomous variable rows
  modify_indent(columns = conf.low, rows = var_type == "dichotomous", indent = 5L)
```

[TABLE]

## Cell Style Modifiers

- [`modify_bold()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md)/[`remove_bold()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md):
  Add/remove bold styling in table cells
- [`modify_italic()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md)/[`remove_italic()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md):
  Add/remove italic styling in table cells

These functions are used to modify the cell font styling within a table.

For each of the `modify_*()` functions the column(s) and row(s) in which
cell contents should be bold/italicized must be specified via the
`columns` and `rows` arguments.

The `remove_*()` functions will remove all bold/italic styling by
default, but cell locations can be specified if needed via the `columns`
and `rows` arguments.

``` r
x |>
  # bold contents of label column in label rows
  modify_bold(columns = label, rows = row_type == "label") |>
  # italicize contents of p.value column in rows where p.value < 0.75
  modify_italic(columns = p.value, rows = p.value < 0.75)
```

[TABLE]

## Value Formatting Modifiers

- [`modify_missing_symbol()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_missing_symbol.md):
  Update how missing values are represented in the table
- [`modify_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_fmt_fun.md):
  Update the formatting functions applied to numeric columns and rows

These functions are used to modify the value formatting within a table.

The primary statistic columns (with column names `stat_*`), in this case
each corresponding to levels of the `by` variable `trt`, are
automatically formatted as character-type columns.

Since these columns are non-numeric functions such as
[`modify_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_fmt_fun.md)
are typically not applied to these columns, and instead should be
applied to numeric columns such as the `estimate`, `conf.low`,
`conf.high`, and `p.value` columns in our example table.

The formatting function modifier can update multiple formats at once by
listing the formats that should be applied in each column, as shown in
the following example.

For tables with merged columns, formatting functions are applied
*before* merging so formatting functions can be applied independently to
each variable included in the merged column.

For example, the confidence interval column in our table is the result
of merging `conf.low` and `conf.high` so to apply a formatting function
to both CI values both `conf.low` and `conf.high` must be specified as
column names in the formatting expression despite belonging to the same
merged column.

The function can be called consecutively if different row subsets should
be updated in each column.

``` r
x |>
  # update missing value symbol from '' to 'NA' for the p.value column in label rows
  modify_missing_symbol(symbol = "<NA>", columns = p.value, rows = row_type == "label") |>
  # update formatting functions in p.value, estimate, conf.low, and conf.high columns for response variable rows
  modify_fmt_fun(
    p.value = label_style_pvalue(digits = 3),
    c(estimate, conf.low, conf.high) ~ label_style_sigfig(digits = 4),
    rows = variable == "response"
  ) |>
  # update formatting function in estimate column for age variable rows
  modify_fmt_fun(
    estimate = label_style_sigfig(digits = 6),
    rows = variable == "age"
  )
```

[TABLE]

## Header Modifiers

- [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md):
  Update column headers
- [`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)/[`remove_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md):
  Update/remove spanning headers

These functions are used to modify table headers.

See the function documentation for details on the customization options
available when modifying headers.

``` r
x |>
  # update header for the estimate column, remove header from label column
  modify_header(
    estimate ~ "**Std. Mean Diff.**",
    label ~ ""
  ) |>
  # add a spanning header across all stat columns
  modify_spanning_header(all_stat_cols() ~ "**Treatment Received (N = {N})**")
```

[TABLE]

## Footnote Modifiers

- [`modify_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)/[`remove_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md):
  Update/remove footnotes corresponding to columns/rows/cells in the
  table
- [`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)/[`remove_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md):
  Update/remove footnotes corresponding to the table header
- [`modify_footnote_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)/[`remove_footnote_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md):
  Update/remove footnotes corresponding to a table spanning header

These functions are used to modify table footnotes.

Each function corresponds to footnotes relating to a specific part of
the table - the table body (including labels), header, and spanning
header(s).

Footnotes will be listed (and numbered) in the order in which they
appear in the table, from top-left to bottom-right. Duplicate footnotes
will only be numbered and displayed once.

By default any pre-existing footnotes will be overwritten and replaced
with the newly specified footnotes. This can be disabled so that
previous footnotes are retained by setting `replace = FALSE` in each of
the footnote modifier functions.

The `remove_*()` functions will remove all footnotes for the associated
table part by default.

See the function documentation for custom footnote removal options.

``` r
x |>
  modify_spanning_header(all_stat_cols() ~ "**Treatment Received (N = {N})**") |>
  # add footnote associated with the grade label within the label column
  modify_footnote_body(
    footnote = "Tumor grade was assessed _before_ treatment began",
    columns = "label",
    rows = variable == "grade" & row_type == "label"
  ) |>
  # add footnote associated with cells in the p.value column with p.value > 0.75
  modify_footnote_body(
    footnote = "Reported p-value outside of the range of interest",
    columns = p.value,
    rows = p.value > 0.75
  ) |>
  # add footnote associated with the header over all columns
  modify_footnote_header(
    footnote = "All subjects received treatment",
    columns = everything(),
    replace = FALSE
  ) |>
  # add footnote associated with spanning header across all stat columns
  modify_footnote_spanning_header(
    "Randomized Treatment",
    columns = all_stat_cols()
  )
```

[TABLE]

## Source Note & Caption Modifiers

- [`modify_abbreviation()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md)/[`remove_abbreviation()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md):
  Update/remove table text abbreviations source note
- [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md):
  Update table caption
- [`modify_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_source_note.md)/[`remove_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_source_note.md):
  Update/remove table source notes

These functions are used to modify table source notes and captions.
Source notes are similar to footnotes but are not linked to a specific
cell in the table.

All abbreviations supplied via
[`modify_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_source_note.md)
are coalesced into a single source note.

Source notes are displayed after footnotes, with the abbreviations
source note listed before any other source notes.

The formatting and appearance of source notes and captions are largely
dependent on the table engine used, as some features are not supported
by all table engines.

See the individual function documentation for details.

``` r
x |>
  modify_source_note("Results as of June 26, 2015") |>
  modify_abbreviation("I = Grade 1")
```

[TABLE]

## Advanced Modifiers

**These functions should be used by developers or advanced users only.**

- [`modify_table_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_body.md):
  Update `x$table_body` directly
- [`modify_table_styling()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_styling.md):
  Update `x$table_styling` directly
- [`modify_post_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_post_fmt_fun.md):
  Apply a second formatting function after the primary formatting
  functions have been applied

The other - more user-friendly - modifier functions are intended to
replace the functionality provided by these functions.

For details on how to use these advanced functions see their function
documentation.
