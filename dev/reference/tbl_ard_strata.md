# Stratified gtsummary tables from ARD

**\[experimental\]**  
Similar to
[`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md),
except the function accepts an ARD instead of a data frame.

## Usage

``` r
tbl_ard_strata(
  cards,
  strata,
  .tbl_fun,
  ...,
  .sep = ", ",
  .combine_with = c("tbl_merge", "tbl_stack"),
  .combine_args = NULL,
  .header = ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}")
)

tbl_ard_strata2(
  cards,
  strata,
  .tbl_fun,
  ...,
  .sep = ", ",
  .combine_with = c("tbl_merge", "tbl_stack"),
  .combine_args = NULL,
  .header = ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}")
)
```

## Arguments

- cards:

  (`card`)  
  An ARD object of class `"card"` typically created with
  `cards::ard_*()` functions.

- strata:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  the grouping columns to stratify by. Must select `'group#'` and
  `'group#_level'` pairs. Importantly, the function expects the
  `'group#'` columns to be the same variable, e.g. stratifying by a
  single variable. The `'group#_level'` value is available to place in
  header (and more) via the `{strata}` element.

- .tbl_fun:

  (`function`) A function or formula. If a *function*, it is used as is.
  If a formula, e.g. `~ .x %>% tbl_summary() %>% add_p()`, it is
  converted to a function. The stratified data frame is passed to this
  function.

- ...:

  Additional arguments passed on to the `.tbl_fun` function.

- .sep:

  (`string`)  
  when more than one stratifying variable is passed, this string is used
  to separate the levels in the spanning header. Default is `", "`

- .combine_with:

  (`string`)  
  One of `c("tbl_merge", "tbl_stack")`. Names the function used to
  combine the stratified tables.

- .combine_args:

  (named `list`)  
  named list of arguments that are passed to function specified in
  `.combine_with`

- .header:

  (`string`)  
  String indicating the headers that will be placed. Default is
  `"**{strata}**"` when `.combine_with = "tbl_merge"` and `"{strata}"`
  when `.combine_with = "tbl_stack"`. Items placed in curly brackets
  will be evaluated according to
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  syntax.

      - `strata` stratum levels

      - `n` N within stratum

      - `N` Overall N

  The evaluated value of `.header` is also available within
  `tbl_strata2(.tbl_fun)`

## Value

a 'gtsummary' table

## Examples

``` r
cards::ADLB |>
  dplyr::filter(
    AVISIT %in% c("Baseline", "Week 12", "Week 24"),
    PARAMCD %in% c("ALB", "BUN")
  ) |>
  cards::ard_summary(
    strata = PARAM,
    by = TRTA,
    variables = AVAL
  ) |>
  tbl_ard_strata2(
    strata = c(group2, group2_level),
    ~ .x |>
      tbl_ard_summary(by = TRTA, label = list(AVAL = .y)),
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL)
  )


  

Characteristic
```

**Placebo**¹

**Xanomeline High Dose**¹

**Xanomeline Low Dose**¹

Albumin (g/L)

39.0 (38.0, 40.0)

41.0 (39.0, 43.0)

40.0 (38.0, 42.0)

Blood Urea Nitrogen (mmol/L)

5.0 (4.3, 6.4)

4.6 (4.3, 6.1)

6.1 (5.0, 7.5)

¹ Median (Q1, Q3)
