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
#> Called from: .tbl_ard_strata_internal(cards = cards, strata = {
#>     {
#>         strata
#>     }
#> }, .tbl_fun = .tbl_fun, ..., .sep = .sep, .combine_with = .combine_with, 
#>     .combine_args = .combine_args, .header = .header, .parent_fun = "tbl_ard_strata2")
#> debug: df_tbls <- dplyr::mutate(dplyr::ungroup(dplyr::mutate(dplyr::rowwise(dplyr::rename(dplyr::arrange(tidyr::nest(card_renamed, 
#>     data = -all_of(.env$strata_renamed)), !!!syms(strata_renamed)), 
#>     !!!syms(new_strata_names))), data = list(cards::as_card(.data$data)), 
#>     strata = paste(!!!syms(names(new_strata_names)), sep = .sep), 
#>     header = glue::glue(.header))), tbl = switch(.parent_fun, 
#>     tbl_ard_strata = map(.data$data, .tbl_fun, ...), tbl_ard_strata2 = map2(.data$data, 
#>         .data$header, .tbl_fun, ...)))
#> debug: df_tbls$tbl_id <- dplyr::pull(dplyr::mutate(df_tbls[names(new_strata_names)], 
#>     across(everything(), .fns = ~paste(new_strata_names[[dplyr::cur_column()]], 
#>         cli::cli_format(.x), sep = "=")), strata = paste(!!!syms(names(new_strata_names)), 
#>         sep = ",")), "strata")
#> debug: .combine_args <- utils::modifyList(switch(.combine_with, tbl_merge = list(tab_spanner = df_tbls$header), 
#>     tbl_stack = list(group_header = df_tbls$header)), val = .combine_args %||% 
#>     list())
#> debug: if (.combine_with == "tbl_merge") {
#>     tbl <- inject(tbl_merge(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, 
#>         !!!.combine_args))
#> } else if (.combine_with == "tbl_stack") {
#>     tbl <- inject(tbl_stack(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, 
#>         !!!.combine_args, tbl_id_lbls = df_tbls$strata))
#> }
#> debug: if (.combine_with == "tbl_stack") {
#>     tbl <- inject(tbl_stack(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, 
#>         !!!.combine_args, tbl_id_lbls = df_tbls$strata))
#> }
#> debug: tbl <- inject(tbl_stack(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, 
#>     !!!.combine_args, tbl_id_lbls = df_tbls$strata))
#> debug: tbl$df_strata <- df_tbls %>% dplyr::select(starts_with("strata_"), 
#>     "header")
#> debug: class(tbl) <- c("tbl_ard_strata", .combine_with, "gtsummary")
#> debug: tbl


  

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
