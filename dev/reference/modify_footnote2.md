# Modify Footnotes

Modify Footnotes

## Usage

``` r
modify_footnote_header(
  x,
  footnote,
  columns,
  replace = TRUE,
  text_interpret = c("md", "html")
)

modify_footnote_body(
  x,
  footnote,
  columns,
  rows,
  replace = TRUE,
  text_interpret = c("md", "html")
)

modify_footnote_spanning_header(
  x,
  footnote,
  columns,
  level = 1L,
  replace = TRUE,
  text_interpret = c("md", "html")
)

remove_footnote_header(x, columns = everything())

remove_footnote_body(x, columns = everything(), rows = TRUE)

remove_footnote_spanning_header(x, columns = everything(), level = 1L)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- footnote:

  (`string`)  
  a string

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  columns to add footnote.

  For `modify_footnote_spanning_header()`, pass a single column name
  where the spanning header begins. If multiple column names are passed,
  only the first is used.

- replace:

  (scalar `logical`)  
  Logical indicating whether to replace any existing footnotes in the
  specified location with the specified footnote, or whether the
  specified should be added to the existing footnote(s) in the
  header/cell. Default is to replace existing footnotes.

- text_interpret:

  (`string`)  
  String indicates whether text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default) or `"html"`. Applies to tables printed with `{gt}`.

- rows:

  (predicate `expression`)  
  Predicate expression to select rows in `x$table_body`. Review [rows
  argument
  details](https://www.danieldsjoberg.com/gtsummary/dev/reference/rows_argument.md).

- level:

  (`integer`)  
  An integer specifying which level to place the spanning header
  footnote.

## Value

Updated gtsummary object

## Examples

``` r
# Example 1 ----------------------------------
tbl <- trial |>
  tbl_summary(by = trt, include = c(age, grade), missing = "no") |>
  modify_footnote_header(
    footnote = "All but four subjects received both treatments in a crossover design",
    columns = all_stat_cols(),
    replace = FALSE
  ) |>
  modify_footnote_body(
    footnote = "Tumor grade was assessed _before_ treatment began",
    columns = "label",
    rows = variable == "grade" & row_type == "label"
  )
tbl


  

Characteristic
```

**Drug A**  
N = 98^(1,2)

**Drug B**  
N = 102^(1,2)

Age

46 (37, 60)

48 (39, 56)

Grade³

  

  

    I

35 (36%)

33 (32%)

    II

32 (33%)

36 (35%)

    III

31 (32%)

33 (32%)

¹ Median (Q1, Q3); n (%)

² All but four subjects received both treatments in a crossover design

³ Tumor grade was assessed *before* treatment began

\# Example 2 ---------------------------------- \# remove all footnotes
tbl \|\> remove_footnote_header(columns =
[all_stat_cols](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)())
\|\> remove_footnote_body(columns = label, rows = variable == "grade" &
row_type == "label")

[TABLE]
