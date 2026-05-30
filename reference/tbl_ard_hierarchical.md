# ARD Hierarchical Table

**\[experimental\]**  
*This is an preview of this function. There will be changes in the
coming releases, and changes will not undergo a formal deprecation
cycle.*

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

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  used to specify the summary statistics to display for all variables in
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_hierarchical.md).
  The default is `everything() ~ "{n} ({p})"`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.md))  
  used to override default labels in hierarchical table, e.g.
  `list(AESOC = "System Organ Class")`. The default for each variable is
  the column label attribute, `attr(., 'label')`. If no label has been
  set, the column name is used.

## Value

a gtsummary table of class `"tbl_ard_hierarchical"`

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
    id = USUBJID
  )

# Second, build table from the ARD
tbl_ard_hierarchical(
  cards = ard,
  variables = c(AESOC, AETERM),
  by = TRTA
)


  

AESOC

    AETERM
```
