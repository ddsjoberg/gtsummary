# Variable Group Header

Some data are inherently grouped, and should be reported together.
Grouped variables are all indented together. This function indents the
variables that should be reported together while adding a header above
the group.

## Usage

``` r
add_variable_group_header(x, header, variables, indent = 4L)
```

## Arguments

- x:

  (`tbl_summary`)  
  gtsummary object of class `'tbl_summary'`

- header:

  (`string`)  
  string of the header to place above the variable group

- variables:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to group that appear in `x$table_body`. Selected variables
  should be appear consecutively in table.

- indent:

  (`integer`)  
  An integer indicating how many space to indent text. All rows in the
  group will be indented by this amount. Default is `4`.

## Value

a gtsummary table

## Details

This function works by inserting a row into the `x$table_body` and
indenting the group of selected variables. This function cannot be used
in conjunction with all functions in gtsummary; for example,
[`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.md)
will bold the incorrect rows after running this function.

## Examples

``` r
# Example 1 ----------------------------------
set.seed(11234)
data.frame(
  exclusion_age = sample(c(TRUE, FALSE), 20, replace = TRUE),
  exclusion_mets = sample(c(TRUE, FALSE), 20, replace = TRUE),
  exclusion_physician = sample(c(TRUE, FALSE), 20, replace = TRUE)
) |>
  tbl_summary(
    label = list(exclusion_age = "Age",
                 exclusion_mets = "Metastatic Disease",
                 exclusion_physician = "Physician")
  ) |>
  add_variable_group_header(
    header = "Exclusion Reason",
    variables = starts_with("exclusion_")
  ) |>
  modify_caption("**Study Exclusion Criteria**")


  
Study Exclusion Criteria

  
Characteristic
```
