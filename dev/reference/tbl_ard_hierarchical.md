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

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  used to specify the summary statistics to display for all variables in
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md).
  The default is `everything() ~ "{n} ({p})"`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
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

**Placebo**  
N = 86¹

**Xanomeline High Dose**  
N = 84¹

**Xanomeline Low Dose**  
N = 84¹

CARDIAC DISORDERS

2 (2.3%)

3 (3.6%)

0 (0.0%)

    ATRIOVENTRICULAR BLOCK SECOND DEGREE

2 (2.3%)

3 (3.6%)

0 (0.0%)

GASTROINTESTINAL DISORDERS

9 (10.5%)

4 (4.8%)

5 (6.0%)

    DIARRHOEA

9 (10.5%)

4 (4.8%)

5 (6.0%)

GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS

8 (9.3%)

25 (29.8%)

24 (28.6%)

    APPLICATION SITE ERYTHEMA

3 (3.5%)

15 (17.9%)

12 (14.3%)

    APPLICATION SITE PRURITUS

6 (7.0%)

22 (26.2%)

22 (26.2%)

SKIN AND SUBCUTANEOUS TISSUE DISORDERS

9 (10.5%)

14 (16.7%)

15 (17.9%)

    ERYTHEMA

9 (10.5%)

14 (16.7%)

15 (17.9%)

¹ n (%)

\# Example 2: Event Counts ------------------- ard \<-
cards::[ard_stack_hierarchical_count](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack_hierarchical.html)(
data = ADAE_subset, variables =
[c](https://rdrr.io/r/base/c.html)(AESOC, AETERM), by = TRTA,
denominator =
cards::[ADSL](https://insightsengineering.github.io/cards/latest-tag/reference/adam.html)
) tbl_ard_hierarchical( cards = ard, variables =
[c](https://rdrr.io/r/base/c.html)(AESOC, AETERM), by = TRTA, statistic
= ~"{n}" )

[TABLE]
