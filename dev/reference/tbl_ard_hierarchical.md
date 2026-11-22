# ARD Hierarchical Table

**\[experimental\]**  
*This is a preview of this function. There will be changes in the coming
releases, and changes will not undergo a formal deprecation cycle.*

Constructs tables from nested or hierarchical data structures (e.g.
adverse events).

## Usage

``` r
tbl_ard_hierarchical(
  cards,
  variables,
  by = NULL,
  include = everything(),
  statistic = ~"{n} ({p}%)",
  overall_row = FALSE,
  label = NULL
)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- variables:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  character vector or tidy-selector of columns in `data` used to create
  a hierarchy. Hierarchy will be built with variables in the order
  given.

- by:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  a single column from `data`. Summary statistics will be stratified by
  this variable. Default is `NULL`.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  columns from the `variables` argument for which summary statistics
  should be returned (on the variable label rows). Including the last
  element of `variables` has no effect since each level has its own row
  for this variable. The default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  used to specify the summary statistics to display for all variables in
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md).
  The default is `everything() ~ "{n} ({p})"`.

- overall_row:

  (scalar `logical`)  
  whether an overall summary row should be included at the top of the
  table. The default is `FALSE`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  used to override default labels in hierarchical table, e.g.
  `list(AESOC = "System Organ Class")`. The default for each variable is
  the label saved in the ARD (e.g. when created with
  `cards::ard_stack_hierarchical(attributes = TRUE)`). If no label is
  present, the column name is used.

## Value

a gtsummary table of class `"tbl_ard_hierarchical"`

## Overall Row

An overall row can be added to the table as the first row by specifying
`overall_row = TRUE`. The `cards` object must contain the overall
results, which are calculated by
`cards::ard_stack_hierarchical(over_variables = TRUE)` and
`cards::ard_stack_hierarchical_count(over_variables = TRUE)`.

A label for this overall row can be specified by passing an
`'..ard_hierarchical_overall..'` element in `label`.

## Examples

``` r
ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:5]
  )

# Example 1: Event Rates  --------------------
# First, build the ARD
ard <-
  cards::ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(AESOC, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    over_variables = TRUE,
    attributes = TRUE
  )

# Second, build table from the ARD
tbl_ard_hierarchical(
  cards = ard,
  variables = c(AESOC, AETERM),
  by = TRTA,
  overall_row = TRUE,
  label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
)


  

Primary System Organ Class

    Reported Term for the Adverse Event
```
