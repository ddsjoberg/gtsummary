
tbl_ard_strata <- function(card,
                           strata,
                           .tbl_fun,
                           ...,
                           .sep = ", ",
                           .combine_with = c("tbl_merge", "tbl_stack"),
                           .combine_args = NULL,
                           .header =
                             ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}")) {
  # check and process inputs ---------------------------------------------------
  set_cli_abort_call()
  check_class(card, cls = "card")
  cards::process_selectors(card, strata = {{ strata }})

  .ard_strata_col_check(card, strata)
  .combine_with <- match.arg(.combine_with)

  # run `tbl_ard_strata()``
  tbl_strata_internal(
    data = card,
    strata = all_of(strata),
    .tbl_fun = .tbl_fun,
    ...,
    .sep = .sep,
    .combine_with = .combine_with,
    .combine_args = .combine_args,
    .header = .header,
    .parent_fun = "tbl_ard_strata"
  )

}

.ard_strata_col_check <- function(card, strata) {
  # check the passed strata are groups that appear in `card`
  if (!all(strata %in% names(dplyr::select(card, cards::all_ard_groups())))) {
    group_columns <- names(dplyr::select(card, cards::all_ard_groups()))

    if (is_empty(group_columns)) {
      cli::cli_abort("The {.arg card} argument input must contain grouping columns.")
    }
    cli::cli_abort(
      c("The columns selected in the {.arg strata} argument must select grouping columns from {.arg card}.",
        i = " Select from {.val {group_columns}}.")
    )
  }
}
