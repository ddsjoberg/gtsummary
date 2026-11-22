# Add rate differences to hierarchical tables

Add a column of event-rate differences to a table created with
[`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_hierarchical.md)
or
[`tbl_ard_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_ard_hierarchical.md).
For every node in the hierarchy (e.g. each system organ class and each
preferred term) the event rate of a second `by` group is subtracted from
the rate of a first `by` group. This is a wrapper around
[`cards::diff_ard_hierarchical()`](https://pharmaverse.github.io/cards/latest-tag/reference/diff_ard_hierarchical.html).

The table must be stratified by a single `by` variable and its
statistics must include the rate (`p`) statistic (the default for
[`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_hierarchical.md)).

## Usage

``` r
# S3 method for class 'tbl_hierarchical'
add_difference(
  x,
  levels = NULL,
  statistic = "{estimate}%",
  estimate_fun = label_style_number(digits = 1, scale = 100),
  ...
)

# S3 method for class 'tbl_ard_hierarchical'
add_difference(
  x,
  levels = NULL,
  statistic = "{estimate}%",
  estimate_fun = NULL,
  ...
)
```

## Arguments

- x:

  (`tbl_hierarchical`/`tbl_ard_hierarchical`)  
  table created with
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_hierarchical.md)
  or
  [`tbl_ard_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_ard_hierarchical.md).

- levels:

  (`vector`)  
  a length-two vector of the `by` variable levels to compare. The
  difference is calculated as `levels[1]` minus `levels[2]`. This
  argument is required when the `by` variable has more than two levels,
  and when `by` has exactly two levels it is optional and can be used to
  flip the direction of the difference. Default is `NULL`.

- statistic:

  (`string`)  
  a single glue string defining the difference statistic to display. The
  only available element is `{estimate}` (the rate difference). Default
  is `"{estimate}%"`.

- estimate_fun:

  (`function`)  
  a function to round and format the rate difference. For
  `add_difference.tbl_hierarchical()` the default is
  `label_style_number(digits = 1, scale = 100)`. For
  `add_difference.tbl_ard_hierarchical()` the default is `NULL`, meaning
  the formatting function carried in the source ARD is used.

- ...:

  These dots are for future extensions and must be empty.

## Value

a gtsummary table of the same class as `x`

## Examples

``` r
# Example 1 ----------------------------------
# rate difference between two treatment arms
ADAE_subset <- cards::ADAE |>
  dplyr::filter(AESOC %in% unique(cards::ADAE$AESOC)[1:5]) |>
  dplyr::filter(.by = AESOC, AEDECOD %in% unique(cards::ADAE$AEDECOD)[1:5])

tbl_hierarchical(
  data = ADAE_subset,
  variables = c(AESOC, AEDECOD),
  by = TRTA,
  denominator = cards::ADSL,
  id = USUBJID
) |>
  add_difference(levels = c("Xanomeline High Dose", "Placebo"))


  

Primary System Organ Class

    Dictionary-Derived Term
```
