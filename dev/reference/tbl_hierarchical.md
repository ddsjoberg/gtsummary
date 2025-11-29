# Hierarchical Table

**\[experimental\]**  
Use these functions to generate hierarchical tables.

- `tbl_hierarchical()`: Calculates *rates* of events (e.g. adverse
  events) utilizing the `denominator` and `id` arguments to identify the
  rows in `data` to include in each rate calculation. If `variables`
  contains more than one variable and the last variable in `variables`
  is an ordered factor, then rates of events by highest level will be
  calculated.

- `tbl_hierarchical_count()`: Calculates *counts* of events utilizing
  all rows for each tabulation.

## Usage

``` r
tbl_hierarchical(
  data,
  variables,
  id,
  denominator,
  by = NULL,
  include = everything(),
  statistic = everything() ~ "{n} ({p}%)",
  overall_row = FALSE,
  label = NULL,
  digits = NULL
)

tbl_hierarchical_count(
  data,
  variables,
  denominator = NULL,
  by = NULL,
  include = everything(),
  overall_row = FALSE,
  statistic = everything() ~ "{n}",
  label = NULL,
  digits = NULL
)
```

## Arguments

- data:

  (`data.frame`)  
  a data frame.

- variables:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  character vector or tidy-selector of columns in `data` used to create
  a hierarchy. Hierarchy will be built with variables in the order
  given.

- id:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  argument used to subset `data` to identify rows in `data` to calculate
  event rates in `tbl_hierarchical()`.

- denominator:

  (`data.frame`, `integer`)  
  used to define the denominator and enhance the output. The argument is
  required for `tbl_hierarchical()` and optional for
  `tbl_hierarchical_count()`. The `denominator` argument must be
  specified when `id` is used to calculate event rates.

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
  `tbl_hierarchical()`. The default is `everything() ~ "{n} ({p})"`.

- overall_row:

  (scalar `logical`)  
  whether an overall summary row should be included at the top of the
  table. The default is `FALSE`.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  used to override default labels in hierarchical table, e.g.
  `list(AESOC = "System Organ Class")`. The default for each variable is
  the column label attribute, `attr(., 'label')`. If no label has been
  set, the column name is used.

- digits:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  specifies how summary statistics are rounded. Values may be either
  integer(s) or function(s). If not specified, default formatting is
  assigned via
  [`label_style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/label_style.md)
  for statistics `n` and `N`, and `label_style_percent(digits=1)` for
  statistic `p`.

## Value

a gtsummary table of class `"tbl_hierarchical"` (for
`tbl_hierarchical()`) or `"tbl_hierarchical_count"` (for
`tbl_hierarchical_count()`).

## Overall Row

An overall row can be added to the table as the first row by specifying
`overall_row = TRUE`. Assuming that each row in `data` corresponds to
one event record, this row will count the overall number of events
recorded when used in `tbl_hierarchical_count()`, or the overall number
of patients recorded with any event when used in `tbl_hierarchical()`.

A label for this overall row can be specified by passing an
`'..ard_hierarchical_overall..'` element in `label`. Similarly, the
rounding for statistics in the overall row can be modified using the
`digits` argument, again referencing the
`'..ard_hierarchical_overall..'` name.

## Examples

``` r
ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:5]
  )

# Example 1 - Event Rates --------------------
tbl_hierarchical(
  data = ADAE_subset,
  variables = c(AESOC, AETERM),
  by = TRTA,
  denominator = cards::ADSL,
  id = USUBJID,
  digits = everything() ~ list(p = 1),
  overall_row = TRUE,
  label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
)


  

Primary System Organ Class

    Reported Term for the Adverse Event
```

**Placebo**  
N = 86¹

**Xanomeline High Dose**  
N = 84¹

**Xanomeline Low Dose**  
N = 84¹

Any Adverse Event

26 (30.2%)

42 (50.0%)

40 (47.6%)

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

\# Example 2 - Rates by Highest Severity ------ tbl_hierarchical( data =
ADAE_subset \|\>
[mutate](https://dplyr.tidyverse.org/reference/mutate.html)(AESEV =
[factor](https://rdrr.io/r/base/factor.html)(AESEV, ordered = TRUE)),
variables = [c](https://rdrr.io/r/base/c.html)(AESOC, AESEV), by = TRTA,
id = USUBJID, denominator =
cards::[ADSL](https://insightsengineering.github.io/cards/latest-tag/reference/adam.html),
include = AESEV, label = [list](https://rdrr.io/r/base/list.html)(AESEV
= "Highest Severity") ) \#\> ℹ Denominator set by "TRTA" column in
\`denominator\` data frame.

[TABLE]

\# Example 3 - Event Counts ------------------- tbl_hierarchical_count(
data = ADAE_subset, variables =
[c](https://rdrr.io/r/base/c.html)(AESOC, AETERM, AESEV), by = TRTA,
overall_row = TRUE, label =
[list](https://rdrr.io/r/base/list.html)(..ard_hierarchical_overall.. =
"Total Number of AEs") )

[TABLE]
