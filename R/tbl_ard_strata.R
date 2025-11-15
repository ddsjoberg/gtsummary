#' Stratified gtsummary tables from ARD
#'
#' @description `r lifecycle::badge("experimental")`\cr
#' Similar to `tbl_strata()`, except the function accepts an ARD
#' instead of a data frame.
#'
#' @inheritParams tbl_ard_summary
#' @inheritParams tbl_strata
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'  the grouping columns to stratify by.
#'  Must select `'group#'` and `'group#_level'` pairs.
#'  Importantly, the function expects the `'group#'` columns to be the same variable,
#'  e.g. stratifying by a single variable.
#'  The `'group#_level'` value is available to place in header (and more) via the `{strata}` element.
#'
#' @returns a 'gtsummary' table
#' @name tbl_ard_strata
#'
#' @examples
#' cards::ADLB |>
#'   dplyr::filter(
#'     AVISIT %in% c("Baseline", "Week 12", "Week 24"),
#'     PARAMCD %in% c("ALB", "BUN")
#'   ) |>
#'   cards::ard_summary(
#'     strata = PARAM,
#'     by = TRTA,
#'     variables = AVAL
#'   ) |>
#'   tbl_ard_strata2(
#'     strata = c(group2, group2_level),
#'     ~ .x |>
#'       tbl_ard_summary(by = TRTA, label = list(AVAL = .y)),
#'     .combine_with = "tbl_stack",
#'     .combine_args = list(group_header = NULL)
#'   )
NULL

#' @rdname tbl_ard_strata
#' @export
tbl_ard_strata <- function(cards,
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
  .combine_with <- match.arg(.combine_with)

  # run `tbl_ard_strata()`
  .tbl_ard_strata_internal(
    cards = cards,
    strata = {{ strata }},
    .tbl_fun = .tbl_fun,
    ...,
    .sep = .sep,
    .combine_with = .combine_with,
    .combine_args = .combine_args,
    .header = .header,
    .parent_fun = "tbl_ard_strata"
  )
}

#' @rdname tbl_ard_strata
#' @export
tbl_ard_strata2 <- function(cards,
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
  .combine_with <- match.arg(.combine_with)

  # run `tbl_ard_strata()`
  .tbl_ard_strata_internal(
    cards = cards,
    strata = {{ strata }},
    .tbl_fun = .tbl_fun,
    ...,
    .sep = .sep,
    .combine_with = .combine_with,
    .combine_args = .combine_args,
    .header = .header,
    .parent_fun = "tbl_ard_strata2"
  )
}


.tbl_ard_strata_internal <- function(cards,
                                     strata,
                                     .tbl_fun,
                                     ...,
                                     .sep,
                                     .combine_with,
                                     .combine_args,
                                     .header,
                                     .parent_fun) {
  check_string(.header)
  check_class(cards, cls = "card")
  # strata can only be ARD groups
  cards::process_selectors(
    cards |> select(cards::all_ard_groups()),
    strata = {{ strata }}
  )
  .ard_strata_col_check(cards, strata)

  card_renamed <- cards |>
    cards::rename_ard_columns(
      columns = intersect(all_of(strata), c(cards::all_ard_groups("names"), cards::all_ard_variables("names")))
    )
  strata_renamed <- names(card_renamed) |> setdiff(names(cards))

  new_strata_names <-
    as.list(strata_renamed) %>%
    set_names(paste0("strata_", seq_len(length(strata_renamed))))

  df_tbls <- card_renamed |>
    tidyr::nest(data = -all_of(.env$strata_renamed)) |>
    dplyr::arrange(!!!syms(strata_renamed)) |>
    dplyr::rename(!!!syms(new_strata_names)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      data = cards::as_card(.data$data) |> list(),
      strata = paste(!!!syms(names(new_strata_names)), sep = .sep),
      header = glue::glue(.header)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      tbl =
        switch(.parent_fun,
               "tbl_ard_strata" = map(.data$data, .tbl_fun, ...),
               "tbl_ard_strata2" = map2(.data$data, .data$header, .tbl_fun, ...)
        )
    )

  # add the column to be used for the tbl_id
  df_tbls$tbl_id <-
    df_tbls[names(new_strata_names)] |>
    dplyr::mutate(
      across(
        everything(),
        .fns = ~ paste(new_strata_names[[dplyr::cur_column()]], cli::cli_format(.x), sep = "=")
      ),
      strata = paste(!!!syms(names(new_strata_names)), sep = ",")
    ) |>
    dplyr::pull("strata")

  # combining tbls -------------------------------------------------------------
  .combine_args <-
    # default arguments
    switch(.combine_with,
           "tbl_merge" = list(tab_spanner = df_tbls$header),
           "tbl_stack" = list(group_header = df_tbls$header)
    ) |>
    # update with user-passed arguments
    utils::modifyList(val = .combine_args %||% list())

  if (.combine_with == "tbl_merge") {
    tbl <- inject(tbl_merge(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, !!!.combine_args))
  } else if (.combine_with == "tbl_stack") {
    tbl <- inject(tbl_stack(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, !!!.combine_args, tbl_id_lbls = df_tbls$strata))
  }

  # return tbl -----------------------------------------------------------------
  tbl$df_strata <- df_tbls %>% dplyr::select(starts_with("strata_"), "header")
  class(tbl) <- c("tbl_ard_strata", .combine_with, "gtsummary")
  tbl
}


.ard_strata_col_check <- function(cards, strata) {
  if (is_empty(strata)) {
    cli::cli_abort(
      "No columns were selected in the {.arg strata} argument.",
      call = get_cli_abort_call()
    )
  }

  # check the group variables are a single variable
  group_columns <- intersect(strata, names(dplyr::select(cards, cards::all_ard_groups("names"))))
  for (i in seq_along(group_columns)) {
    if (any(cards[[group_columns[i]]] != cards[[group_columns[i]]][1])) {
      cli::cli_abort(
        c("The {.val {group_columns[i]}} column must be the same value for all rows.",
          i = "The following levels are present: {.val {unique(cards[[group_columns[i]]])}}"),
        call = get_cli_abort_call()
      )
    }
  }

}
