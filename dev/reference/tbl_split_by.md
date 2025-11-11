# Split gtsummary table by rows and/or columns

**\[experimental\]**  
The `tbl_split_by_rows()` and `tbl_split_by_columns()` functions split a
single gtsummary table into multiple tables. Both column-wise splitting
(that is, splits by columns in `x$table_body`) and row-wise splitting is
possible.

## Usage

``` r
tbl_split_by_rows(
  x,
  variables = NULL,
  row_numbers = NULL,
  variable_level = NULL,
  footnotes = c("all", "first", "last"),
  caption = c("all", "first", "last")
)

tbl_split_by_columns(
  x,
  keys,
  groups,
  footnotes = c("all", "first", "last"),
  caption = c("all", "first", "last")
)

# S3 method for class 'tbl_split'
print(x, ...)
```

## Arguments

- x:

  (`gtsummary` or `list`)  
  gtsummary table.

- variables, row_numbers, variable_level:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  or `integer`)  
  Specifies where the table will be split.

  - `variables`: Tables will be separated after each of the variables
    specified. The `x$table_body` data frame must contains a
    `'variable'` column to use this argument.

  - `row_numbers`: Row numbers after which the table will be split.

  - `variable_level`: A single column name in `x$table_body`. When
    specified, the table will be split at each unique level of the
    variable.

- footnotes, caption:

  (`string`) **\[experimental\]**  
  can be either `"first"`, `"all"`, or `"last"`, to locate global
  footnotes or caption only on the first, in each, or in the last table,
  respectively. It defaults to `"all"`. Reference footnotes are always
  present wherever they appear.

- keys:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  columns to be repeated in each table split. It defaults to the first
  column if missing (usually label column).

- groups:

  (list of `character` vectors)  
  list of column names that appear in `x$table_body`. Each group of
  column names represent a different table in the output list.

- ...:

  These dots are for future extensions and must be empty.

## Value

`tbl_split` object. If multiple splits are performed (e.g., both by row
and columns), the output is returned a single level list.

## Details

Run
[`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
to print all column names to split by.

Footnotes and caption handling are experimental and may change in the
future.

`row_numbers` indicates the row numbers at which to split the table. It
means that the table will be split after each of these row numbers. If
the last row is selected, the split will not happen as it is supposed to
happen after the last row.

## Examples

``` r
# Example 1 ----------------------------------
# Split by rows
trial |>
  tbl_summary(by = trt) |>
  tbl_split_by_rows(variables = c(marker, grade)) |>
  dplyr::last() # Print only last table for simplicity


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

Tumor Response

28 (29%)

33 (34%)

    Unknown

3

4

Patient Died

52 (53%)

60 (59%)

Months to Death/Censor

23.5 (17.4, 24.0)

21.2 (14.5, 24.0)

¹ Median (Q1, Q3); n (%)

\# Example 2 ---------------------------------- \# Split by rows with
row numbers trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(by
= trt) \|\> tbl_split_by_rows(row_numbers =
[c](https://rdrr.io/r/base/c.html)(5, 7)) \|\>
dplyr::[last](https://dplyr.tidyverse.org/reference/nth.html)() \# Print
only last table for simplicity

[TABLE]

\# Example 3 ---------------------------------- \# Split by columns
trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(by
= trt, include = [c](https://rdrr.io/r/base/c.html)(death, ttdeath))
\|\> tbl_split_by_columns(groups =
[list](https://rdrr.io/r/base/list.html)("stat_1", "stat_2")) \|\>
dplyr::[last](https://dplyr.tidyverse.org/reference/nth.html)() \# Print
only last table for simplicity

[TABLE]

\# Example 4 ---------------------------------- \# Both row and column
splitting trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(by
= trt) \|\> tbl_split_by_rows(variables =
[c](https://rdrr.io/r/base/c.html)(marker, grade)) \|\>
tbl_split_by_columns(groups =
[list](https://rdrr.io/r/base/list.html)("stat_1", "stat_2")) \|\>
dplyr::[last](https://dplyr.tidyverse.org/reference/nth.html)() \# Print
only last table for simplicity

[TABLE]

\# Example 5 ------------------------------ \# Split by rows with
footnotes and caption trial \|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)(by
= trt, missing = "no") \|\>
[modify_footnote_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)(
footnote = "All but four subjects received both treatments in a
crossover design", columns =
[all_stat_cols](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)(),
replace = FALSE ) \|\>
[modify_footnote_body](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)(
footnote = "Tumor grade was assessed \_before\_ treatment began",
columns = "label", rows = variable == "grade" & row_type == "label" )
\|\>
[modify_spanning_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)(
[c](https://rdrr.io/r/base/c.html)(stat_1, stat_2) ~ "\*\*TRT\*\*" )
\|\>
[modify_abbreviation](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md)("I
= 1, II = 2, III = 3") \|\>
[modify_caption](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)("\_Some
caption\_") \|\>
[modify_footnote_spanning_header](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)(
footnote = "Treatment", columns =
[c](https://rdrr.io/r/base/c.html)(stat_1) ) \|\>
[modify_source_note](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_source_note.md)("Some
source note!") \|\> tbl_split_by_rows(variables =
[c](https://rdrr.io/r/base/c.html)(marker, stage, grade), footnotes =
"last", caption = "first") \|\>
dplyr::[nth](https://dplyr.tidyverse.org/reference/nth.html)(n = 2) \#
Print only one but not last table for simplicity

[TABLE]
