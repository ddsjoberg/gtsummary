# ARD summary table

**\[experimental\]**  
The `tbl_ard_summary()` function tables descriptive statistics for
continuous, categorical, and dichotomous variables. The functions
accepts an ARD object.

## Usage

``` r
tbl_ard_summary(
  cards,
  by = NULL,
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~
    "{n} ({p}%)"),
  type = NULL,
  label = NULL,
  missing = c("no", "ifany", "always"),
  missing_text = "Unknown",
  missing_stat = "{N_miss}",
  include = everything(),
  overall = FALSE
)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- by:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  A single column from `data`. Summary statistics will be stratified by
  this variable. Default is `NULL`

- statistic:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to specify the summary statistics for each variable. Each of the
  statistics must be present in `card` as no new statistics are
  calculated in this function. The default is
  `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.

- type:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Specifies the summary type. Accepted value are
  `c("continuous", "continuous2", "categorical", "dichotomous")`.
  Continuous summaries may be assigned `c("continuous", "continuous2")`,
  while categorical and dichotomous cannot be modified.

- label:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/dev/reference/syntax.md))  
  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- missing, missing_text, missing_stat:

  Arguments dictating how and if missing values are presented:

  - `missing`: must be one of `c("no", "ifany", "always")`

  - `missing_text`: string indicating text shown on missing row. Default
    is `"Unknown"`

  - `missing_stat`: statistic to show on missing row. Default is
    `"{N_miss}"`. Possible values are `N_miss`, `N_obs`, `N_nonmiss`,
    `p_miss`, `p_nonmiss`

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include in the summary table. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)

- overall:

  (scalar `logical`)  
  When `TRUE`, the `cards` input is parsed into two parts to run
  `tbl_ard_summary(cards_by) |> add_overall(cards_overall)`. Can only by
  used when `by` argument is specified. Default is `FALSE`.

## Value

a gtsummary table of class `"tbl_ard_summary"`

## Details

There are three types of additional data that can be included in the ARD
to improve the default appearance of the table.

1.  **Attributes**: When attributes are included, the default labels
    will be the variable labels, when available. Attributes can be
    included in an ARD with
    [`cards::ard_attributes()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_attributes.html)
    or `ard_stack(.attributes = TRUE)`.

2.  **Missing**: When missing results are included, users can include
    missing counts or rates for variables with
    `tbl_ard_summary(missing = c("ifany", "always"))`. The missing
    statistics can be included in an ARD with
    [`cards::ard_missing()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_missing.html)
    or `ard_stack(.missing = TRUE)`.

3.  **Total N**: The total N is saved internally when available, and it
    can be calculated with
    [`cards::ard_total_n()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_total_n.html)
    or `ard_stack(.total_n = TRUE)`.

## Examples

``` r
library(cards)

ard_stack(
  data = ADSL,
  ard_tabulate(variables = "AGEGR1"),
  ard_summary(variables = "AGE"),
  .attributes = TRUE,
  .missing = TRUE,
  .total_n = TRUE
) |>
  tbl_ard_summary()


  

Characteristic
```

**Overall**¹

Pooled Age Group 1

  

    65-80

144 (56.7%)

    \<65

33 (13.0%)

    \>80

77 (30.3%)

Age

77.0 (70.0, 81.0)

¹ n (%); Median (Q1, Q3)

[ard_stack](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html)(
data = ADSL, .by = ARM,
[ard_tabulate](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate.html)(variables
= "AGEGR1"),
[ard_summary](https://insightsengineering.github.io/cards/latest-tag/reference/ard_summary.html)(variables
= "AGE"), .attributes = TRUE, .missing = TRUE, .total_n = TRUE ) \|\>
tbl_ard_summary(by = ARM)

[TABLE]

[ard_stack](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html)(
data = ADSL, .by = ARM,
[ard_tabulate](https://insightsengineering.github.io/cards/latest-tag/reference/ard_tabulate.html)(variables
= "AGEGR1"),
[ard_summary](https://insightsengineering.github.io/cards/latest-tag/reference/ard_summary.html)(variables
= "AGE"), .attributes = TRUE, .missing = TRUE, .total_n = TRUE, .overall
= TRUE ) \|\> tbl_ard_summary(by = ARM, overall = TRUE)

[TABLE]
