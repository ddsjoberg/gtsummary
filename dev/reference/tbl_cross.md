# Cross table

The function creates a cross table of categorical variables.

## Usage

``` r
tbl_cross(
  data,
  row = 1L,
  col = 2L,
  label = NULL,
  statistic = ifelse(percent == "none", "{n}", "{n} ({p}%)"),
  digits = NULL,
  percent = c("none", "column", "row", "cell"),
  margin = c("column", "row"),
  missing = c("ifany", "always", "no"),
  missing_text = "Unknown",
  margin_text = "Total"
)
```

## Arguments

- data:

  (`data.frame`)  
  A data frame.

- row:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Column name in `data` to be used for the rows of cross table. Default
  is the first column in `data`.

- col:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Column name in `data` to be used for the columns of cross table.
  Default is the second column in `data`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- statistic:

  (`string`)  
  A string with the statistic name in curly brackets to be replaced with
  the numeric statistic (see glue::glue). The default is `{n}`. If
  percent argument is `"column"`, `"row"`, or `"cell"`, default is
  `"{n} ({p}%)"`.

- digits:

  (`numeric`/`list`/`function`)  
  Specifies the number of decimal places to round the summary
  statistics. This argument is passed to
  `tbl_summary(digits = ~digits)`. By default integers are shown to the
  zero decimal places, and percentages are formatted with
  [`style_percent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_percent.md).
  If you would like to modify either of these, pass a vector of integers
  indicating the number of decimal places to round the statistics. For
  example, if the statistic being calculated is `"{n} ({p}%)"` and you
  want the percent rounded to 2 decimal places use `digits = c(0, 2)`.
  User may also pass a styling function: `digits = style_sigfig`

- percent:

  (`string`)  
  Indicates the type of percentage to return. Must be one of "none",
  "column", "row", or "cell". Default is "cell" when `{N}` or `{p}` is
  used in statistic.

- margin:

  (`character`)  
  Indicates which margins to add to the table. Default is
  `c("row", "column")`. Use `margin = NULL` to suppress both row and
  column margins.

- missing:

  (`string`)  
  Must be one of `c("ifany", "no", "always")`.

- missing_text:

  (`string`)  
  String indicating text shown on missing row. Default is `"Unknown"`

- margin_text:

  (`string`)  
  Text to display for margin totals. Default is `"Total"`

## Value

A `tbl_cross` object

## Author

Karissa Whiting, Daniel D. Sjoberg

## Examples

``` r
# Example 1 ----------------------------------
trial |>
  tbl_cross(row = trt, col = response) |>
  bold_labels()


  

```

**Tumor Response**

**Total**

0

1

Unknown

Chemotherapy Treatment

  

  

  

  

    Drug A

67

28

3

98

    Drug B

65

33

4

102

Total

132

61

7

200

\# Example 2 ---------------------------------- trial \|\> tbl_cross(row
= stage, col = trt, percent = "cell") \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)()
\|\>
[bold_labels](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)()

[TABLE]
