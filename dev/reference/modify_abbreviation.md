# Modify Abbreviations

All abbreviations will be coalesced when printing the final table into a
single source note.

## Usage

``` r
modify_abbreviation(x, abbreviation, text_interpret = c("md", "html"))

remove_abbreviation(x, abbreviation = NULL)
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- abbreviation:

  (`string`)  
  a string. In `remove_abbreviation()`, the default value is `NULL`,
  which will remove all abbreviation source notes.

- text_interpret:

  (`string`)  
  String indicates whether text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default) or `"html"`. Applies to tables printed with `{gt}`.

## Value

Updated gtsummary object

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

**Drug A**  
N = 98

**Drug B**  
N = 102

Age

  

  

    Median (IQR)

46 (37, 60)

48 (39, 56)

    Unknown

7

4

Abbreviation: IQR = Interquartile Range

\# Example 2 ----------------------------------
[lm](https://rdrr.io/r/stats/lm.html)(marker ~ trt, trial) \|\>
[tbl_regression](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)()
\|\> remove_abbreviation("CI = Confidence Interval")

[TABLE]
