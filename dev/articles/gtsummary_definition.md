# Definition of a gtsummary Object

This vignette is meant for those who wish to contribute to {gtsummary},
or users who wish to gain an understanding of the inner-workings of a
{gtsummary} object so they may more easily modify them to suit your own
needs. If this does not describe you, please refer to the [{gtsummary}
website](https://www.danieldsjoberg.com/gtsummary/) to an introduction
on how to use the package’s functions and tutorials on advanced use.

*This overview is for informational purposes. Documenting the internal
structures does not mean the internal structures of the package won’t be
updated. If they are updated, this is not considered a breaking change.*

## Introduction

Every {gtsummary} table has a few characteristics common among all
tables created with the package. Here, we review those characteristics,
and provide instructions on how to construct a {gtsummary} object.

We begin by creating two common {gtsummary} tables to be used as
illustrative examples.

``` r
library(gtsummary)

tbl_regression_ex <-
  lm(age ~ grade + marker, trial) |> 
  tbl_regression() |> 
  bold_p(t = 0.5)

tbl_summary_ex <-
  trial |> 
  tbl_summary(by = trt, include = c(trt, age, grade, response))
```

## Structure of a {gtsummary} object

Every {gtsummary} object is a list comprising of, at minimum, these
elements:

``` r
.$table_body    .$table_styling         
```

#### table_body

The `.$table_body` object is the data frame that will ultimately be
printed as the output. To utilize all functions that work with
{gtsummary} tables (e.g. {gtsummary} selectors), the table must include
columns `"label"`, `"row_type"`, and `"variable"`. The `"label"` column
is printed, and the other two are hidden from the final output.

``` r
tbl_summary_ex$table_body
#> # A tibble: 8 × 7
#>   variable var_type    row_type var_label      label          stat_1      stat_2
#>   <chr>    <chr>       <chr>    <chr>          <chr>          <chr>       <chr> 
#> 1 age      continuous  label    Age            Age            46 (37, 60) 48 (3…
#> 2 age      continuous  missing  Age            Unknown        7           4     
#> 3 grade    categorical label    Grade          Grade          NA          NA    
#> 4 grade    categorical level    Grade          I              35 (36%)    33 (3…
#> 5 grade    categorical level    Grade          II             32 (33%)    36 (3…
#> 6 grade    categorical level    Grade          III            31 (32%)    33 (3…
#> 7 response dichotomous label    Tumor Response Tumor Response 28 (29%)    33 (3…
#> 8 response dichotomous missing  Tumor Response Unknown        3           4
```

#### table_styling

The `.$table_styling` object is a list of data frames containing
information about how `.$table_body` is printed, formatted, and
styled.  
The list contains the following data frames `header`, `footnote_header`,
`footnote_body`, `footnote_spanning_header`, `abbreviation`,
`source_note`, `fmt_fun`, `text_format`, `fmt_missing`, `cols_merge` and
the following objects `caption` and `horizontal_line_above`.

**`header`**

The `header` table has the following columns and is one row per column
found in `.$table_body`. The table contains styling information that
applies to entire column or the columns headers.

| Column                | Description                                                                                                                                                                                                                            |
|-----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| column                | Column name from `.$table_body`                                                                                                                                                                                                        |
| hide                  | Logical indicating whether the column is hidden in the output. This column is also scoped in [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md) (and friends) to be used in a selecting environment |
| align                 | Specifies the alignment/justification of the column, e.g. ‘center’ or ‘left’                                                                                                                                                           |
| label                 | Label that will be displayed (if column is displayed in output)                                                                                                                                                                        |
| interpret_label       | the {gt} function that is used to interpret the column label, [`gt::md()`](https://gt.rstudio.com/reference/md.html) or [`gt::html()`](https://gt.rstudio.com/reference/html.html)                                                     |
| modify_stat\_{\*}     | any column beginning with `modify_stat_` is a statistic available to report in [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md) (and others)                                                      |
| modify_selector\_{\*} | any column beginning with `modify_selector_` is a column that is scoped in [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md) (and friends) to be used in a selecting environment                   |

**`footnote_header`**

Each {gtsummary} table may contain footnotes in the column headers.
Updates/changes to footnote are appended to the bottom of the tibble.

| Column         | Description                                                                                                                                                                       |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| column         | Column name from `.$table_body`                                                                                                                                                   |
| footnote       | string containing footnote to add to column/row                                                                                                                                   |
| text_interpret | the {gt} function that is used to interpret the source note, [`gt::md()`](https://gt.rstudio.com/reference/md.html) or [`gt::html()`](https://gt.rstudio.com/reference/html.html) |
| replace        | logical indicating whether this footnote should replace any existing footnote in that header (TRUE) or be added to any existing (FALSE)                                           |
| remove         | logical indicating whether to remove all footnotes in the column header                                                                                                           |

**`footnote_body`**

Each {gtsummary} table may include footnotes in the body of the table.
Updates/changes to footnote are appended to the bottom of the tibble.

| Column         | Description                                                                                                                                                                       |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| column         | Column name from `.$table_body`                                                                                                                                                   |
| rows           | expression selecting rows in `.$table_body`, `NA` indicates to add footnote to header                                                                                             |
| footnote       | string containing footnote to add to column/row                                                                                                                                   |
| text_interpret | the {gt} function that is used to interpret the source note, [`gt::md()`](https://gt.rstudio.com/reference/md.html) or [`gt::html()`](https://gt.rstudio.com/reference/html.html) |
| replace        | logical indicating whether this footnote should replace any existing footnote in that header (TRUE) or be added to any existing (FALSE)                                           |
| remove         | logical indicating whether to remove all footnotes in the column header                                                                                                           |

**`footnote_spanning_header`**

Each {gtsummary} table may include footnotes in the spanning headers of
the table. Updates/changes to footnote are appended to the bottom of the
tibble.

| Column         | Description                                                                                                                                                                       |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| level          | Integer specifying the spanning header level, similar to `gt::tab_spanner(level)`                                                                                                 |
| column         | Column name from `.$table_body`                                                                                                                                                   |
| footnote       | string containing footnote to add to column/row                                                                                                                                   |
| text_interpret | the {gt} function that is used to interpret the source note, [`gt::md()`](https://gt.rstudio.com/reference/md.html) or [`gt::html()`](https://gt.rstudio.com/reference/html.html) |
| replace        | logical indicating whether this footnote should replace any existing footnote in that header (TRUE) or be added to any existing (FALSE)                                           |
| remove         | logical indicating whether to remove all footnotes in the column header                                                                                                           |

**`abbreviation`**

Abbreviations are added one at a time, and at the time of table
rendering, they are coalesced into a single source note.

| Column         | Description                                                                                                                                                                       |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| column         | Optional column name from `.$table_body`. When present, the abbreviation is only printed when the column appears in the rendered table                                            |
| abbreviation   | string containing the abbreviation to add                                                                                                                                         |
| text_interpret | the {gt} function that is used to interpret the source note, [`gt::md()`](https://gt.rstudio.com/reference/md.html) or [`gt::html()`](https://gt.rstudio.com/reference/html.html) |

**`source_note`**

A tibble with the source notes to include. Each source note is assigned
an ID based on the order it is added to the table. Source notes are
added one at a time and present much like footnotes, but are not linked
to a header or cell in the table.

| Column         | Description                                                                                                                                                                       |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| id             | Integer idenitfying the source note                                                                                                                                               |
| source_note    | string containing the abbreviation to add                                                                                                                                         |
| text_interpret | the {gt} function that is used to interpret the source note, [`gt::md()`](https://gt.rstudio.com/reference/md.html) or [`gt::html()`](https://gt.rstudio.com/reference/html.html) |
| remove         | logical indicating whether the source note should be included or removed from final table                                                                                         |

**`fmt_fun`**

Numeric columns/rows are styled with the functions stored in `fmt_fun`.
Updates/changes to styling functions are appended to the bottom of the
tibble.

| Column  | Description                                 |
|---------|---------------------------------------------|
| column  | Column name from `.$table_body`             |
| rows    | expression selecting rows in `.$table_body` |
| fmt_fun | list of formatting/styling functions        |

**`text_format`**

Columns/rows are styled with bold, italic, or indenting stored in
`text_format`. Updates/changes to styling functions are appended to the
bottom of the tibble.

| Column           | Description                                                                 |
|------------------|-----------------------------------------------------------------------------|
| column           | Column name from `.$table_body`                                             |
| rows             | expression selecting rows in `.$table_body`                                 |
| format_type      | one of `c('bold', 'italic', 'indent')`                                      |
| undo_text_format | logical indicating where the formatting indicated should be undone/removed. |

**`fmt_missing`**

By default, all `NA` values are shown blanks. Missing values in
columns/rows are replaced with the `symbol`. For example, reference rows
in
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
are shown with an em-dash. Updates/changes to styling functions are
appended to the bottom of the tibble.

| Column | Description                                            |
|--------|--------------------------------------------------------|
| column | Column name from `.$table_body`                        |
| rows   | expression selecting rows in `.$table_body`            |
| symbol | string to replace missing values with, e.g. an em-dash |

**`cols_merge`**

This object is *experimental* and may change in the future. This tibble
gives instructions for merging columns into a single column. The
implementation in
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
will be updated after
[`gt::cols_label()`](https://gt.rstudio.com/reference/cols_label.html)
gains a `rows=` argument.

| Column  | Description                                                                                                            |
|---------|------------------------------------------------------------------------------------------------------------------------|
| column  | Column name from `.$table_body`                                                                                        |
| rows    | expression selecting rows in `.$table_body`                                                                            |
| pattern | glue pattern directing how to combine/merge columns. The merged columns will replace the column indicated in ‘column’. |

**`caption`**

String that is made into the table caption. The attribute
`"text_interpret"` is either `c("md", "html")`.

**`horizontal_line_above`**

Expression identifying a row where a horizontal line is placed above in
the table.

Example from
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)

``` r
tbl_regression_ex$table_styling
#> $header
#> # A tibble: 24 × 6
#>    column             hide  align  interpret_label label           modify_stat_N
#>    <chr>              <lgl> <chr>  <chr>           <chr>                   <int>
#>  1 variable           TRUE  center gt::md          variable                  179
#>  2 var_label          TRUE  center gt::md          var_label                 179
#>  3 var_type           TRUE  center gt::md          var_type                  179
#>  4 reference_row      TRUE  center gt::md          reference_row             179
#>  5 row_type           TRUE  center gt::md          row_type                  179
#>  6 header_row         TRUE  center gt::md          header_row                179
#>  7 N_obs              TRUE  center gt::md          N_obs                     179
#>  8 N                  TRUE  center gt::md          **N**                     179
#>  9 coefficients_type  TRUE  center gt::md          coefficients_t…           179
#> 10 coefficients_label TRUE  center gt::md          coefficients_l…           179
#> # ℹ 14 more rows
#> 
#> $spanning_header
#> # A tibble: 0 × 5
#> # ℹ 5 variables: level <int>, column <chr>, spanning_header <chr>,
#> #   text_interpret <chr>, remove <lgl>
#> 
#> $footnote_header
#> # A tibble: 0 × 5
#> # ℹ 5 variables: column <chr>, footnote <chr>, text_interpret <chr>,
#> #   replace <lgl>, remove <lgl>
#> 
#> $footnote_body
#> # A tibble: 0 × 6
#> # ℹ 6 variables: column <chr>, rows <list>, footnote <chr>,
#> #   text_interpret <chr>, replace <lgl>, remove <lgl>
#> 
#> $footnote_spanning_header
#> # A tibble: 0 × 6
#> # ℹ 6 variables: column <chr>, footnote <chr>, level <int>,
#> #   text_interpret <chr>, replace <lgl>, remove <lgl>
#> 
#> $abbreviation
#> # A tibble: 2 × 3
#>   column    abbreviation             text_interpret
#>   <chr>     <chr>                    <chr>         
#> 1 conf.low  CI = Confidence Interval gt::md        
#> 2 std.error SE = Standard Error      gt::md        
#> 
#> $source_note
#> # A tibble: 0 × 4
#> # ℹ 4 variables: id <int>, source_note <chr>, text_interpret <chr>,
#> #   remove <lgl>
#> 
#> $text_format
#> # A tibble: 1 × 4
#>   column  rows      format_type undo_text_format
#>   <chr>   <list>    <chr>       <lgl>           
#> 1 p.value <quosure> bold        FALSE           
#> 
#> $indent
#> # A tibble: 2 × 3
#>   column rows      n_spaces
#>   <chr>  <list>       <int>
#> 1 label  <lgl [1]>        0
#> 2 label  <quosure>        4
#> 
#> $fmt_missing
#> # A tibble: 4 × 3
#>   column    rows      symbol
#>   <chr>     <list>    <chr> 
#> 1 estimate  <quosure> —     
#> 2 conf.low  <quosure> —     
#> 3 std.error <quosure> —     
#> 4 statistic <quosure> —     
#> 
#> $fmt_fun
#> # A tibble: 10 × 3
#>    column      rows      fmt_fun
#>    <chr>       <list>    <list> 
#>  1 estimate    <quosure> <fn>   
#>  2 N           <quosure> <fn>   
#>  3 N_obs       <quosure> <fn>   
#>  4 n_obs       <quosure> <fn>   
#>  5 conf.low    <quosure> <fn>   
#>  6 conf.high   <quosure> <fn>   
#>  7 p.value     <quosure> <fn>   
#>  8 std.error   <quosure> <fn>   
#>  9 statistic   <quosure> <fn>   
#> 10 var_nlevels <quosure> <fn>   
#> 
#> $cols_merge
#> # A tibble: 1 × 3
#>   column   rows      pattern                
#>   <chr>    <list>    <chr>                  
#> 1 conf.low <quosure> {conf.low}, {conf.high}
#> 
#> $post_fmt_fun
#> # A tibble: 0 × 3
#> # ℹ 3 variables: column <chr>, rows <list>, fmt_fun <list>
```

## Constructing a {gtsummary} object

#### table_body

When constructing a {gtsummary} object, the author will begin with the
`.$table_body` object. Recall the `.$table_body` data frame must include
columns `"label"`, `"row_type"`, and `"variable"`. Of these columns,
only the `"label"` column will be printed with the final results. The
`"row_type"` column typically will control whether or not the label
column is indented. The `"variable"` column is often used in the
[`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md)
family of functions, and merging {gtsummary} tables with
[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md).

``` r
tbl_regression_ex %>%
  getElement("table_body") %>%
  select(variable, row_type, label)
#> # A tibble: 5 × 3
#>   variable row_type label               
#>   <chr>    <chr>    <chr>               
#> 1 grade    label    Grade               
#> 2 grade    level    I                   
#> 3 grade    level    II                  
#> 4 grade    level    III                 
#> 5 marker   label    Marker Level (ng/mL)
```

The other columns in `.$table_body` are created by the user and are
likely printed in the output. Formatting and printing instructions for
these columns is stored in `.$table_styling`.

### table_styling

There are a few internal {gtsummary} functions to assist in constructing
and modifying a `.$table_header` data frame.

1.  [`as_gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gtsummary.md)
    / `.create_gtsummary_object(table_body)` After a user creates a
    `table_body`, pass it to this function and the skeleton of a
    gtsummary object is created and returned (including the full
    `table_styling` list of tables).

2.  `.update_table_styling()` After columns are added or removed from
    `table_body`, run this function to update `.$table_styling` to
    include or remove styling instructions for the columns. FYI the
    default styling for each new column is to hide it.

3.  [`modify_table_styling()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_styling.md)
    This exported function modifies the printing instructions for a
    single column or groups of columns.

4.  [`modify_table_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_body.md)
    This exported function helps users make changes to `.$table_body`.
    The function runs `.update_table_styling()` internally to maintain
    internal validity with the printing instructions.

## Printing a {gtsummary} object

All {gtsummary} objects are printed with
[`print.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/print_gtsummary.md).
Before a {gtsummary} object is printed, it is converted to a {gt} object
using
[`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md).
This function takes the {gtsummary} object as its input, and uses the
information in `.$table_styling` to construct a list of {gt} calls that
will be executed on `.$table_body`. After the {gtsummary} object is
converted to {gt}, it is then printed as any other {gt} object.

In some cases, the package defaults to printing with other engines, such
as flextable
([`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)),
huxtable
([`as_hux_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)),
kableExtra
([`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)),
and kable
([`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)).
The default print engine is set with the theme element
`"pkgwide-str:print_engine"`

While the actual print function is slightly more involved, it is
basically this:

``` r
print.gtsummary <- function(x) {
  get_theme_element("pkgwide-str:print_engine") %>%
    switch(
      "gt" = as_gt(x),
      "flextable" = as_flex_table(x),
      "huxtable" = as_hux_table(x),
      "kable_extra" = as_kable_extra(x),
      "kable" = as_kable(x)
    ) %>%
    print()
}
```

## The `.$cards` object

When a gtsummary function is called that requires new statistics, these
new calculations are stored in a tibble. These tibbles are often
calculated with functions from the {cards} and {cardx} packages.

These structured tibbles store labels for statistics, functions to
format them, and more. See the {cards} package documentation for
details.

``` r
tbl_summary_ex$cards[["tbl_summary"]]
#> {cards} data frame: 76 x 12
#>    group1 group1_level variable variable_level stat_name stat_label  stat
#> 1     trt       Drug A    grade              I         n          n    35
#> 2     trt       Drug A    grade              I         N          N    98
#> 3     trt       Drug A    grade              I         p          % 0.357
#> 4     trt       Drug A    grade             II         n          n    32
#> 5     trt       Drug A    grade             II         N          N    98
#> 6     trt       Drug A    grade             II         p          % 0.327
#> 7     trt       Drug A    grade            III         n          n    31
#> 8     trt       Drug A    grade            III         N          N    98
#> 9     trt       Drug A    grade            III         p          % 0.316
#> 10    trt       Drug B    grade              I         n          n    33
#> ℹ 66 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 5 more variables: context, fmt_fun, warning, error, gts_column
```
