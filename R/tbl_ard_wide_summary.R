#' Wide ARD summary table
#'
#' `r lifecycle::badge("experimental")`\cr
#' This function is similar to `tbl_ard_summary()`, but places summary statistics
#' wide, in separate columns.
#' All included variables must be of the same summary type, e.g. all continuous
#' summaries or all categorical summaries (which encompasses dichotomous variables).
#'
#' @param cards (`card`)\cr
#'   An ARD object of class `"card"` typically created with `cards::ard_*()` functions.
#' @inheritParams tbl_wide_summary
#'
#' @return a gtsummary table of class 'tbl_wide_summary'
#' @export
#'
#' @examples
#' library(cards)
#'
#' ard_stack(
#'   trial,
#'   ard_continuous(variables = age),
#'   .missing = TRUE,
#'   .attributes = TRUE
#' ) |>
#'   tbl_ard_wide_summary()
#'
#' ard_stack(
#'   trial,
#'   ard_dichotomous(variables = response),
#'   ard_categorical(variables = grade),
#'   .missing = TRUE,
#'   .attributes = TRUE
#' ) |>
#'   tbl_ard_wide_summary()
tbl_ard_wide_summary <- function(cards,
                                 statistic =
                                   switch(type[[1]],
                                          "continuous" = c("{median}", "{p25}, {p75}"),
                                          c("{n}", "{p}%")),
                                 type = NULL,
                                 value = NULL,
                                 include = everything()) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  check_not_missing(cards)
  check_class(cards, "card")
  if (!is_empty(dplyr::select(cards, cards::all_ard_groups()) |> names())) {
    cli::cli_abort(
      "The {.arg cards} object cannot contain grouping variables {.val {dplyr::select(cards, cards::all_ard_groups()) |> names()}}.",
      call = get_cli_abort_call()
    )
  }

  data <- bootstrap_df_from_cards(cards)
  cards::process_selectors(data, include = {{ include }})
  cards::process_formula_selectors(
    data[include],
    type = type,
    value = value
  )
  cards::fill_formula_selectors(
    data[include],
    type =
      cards[c("variable", "context")] |>
      dplyr::filter(.data$context %in% c("continuous", "categorical", "dichotomous")) |>
      unique() |>
      deframe() |>
      as.list()
  )

  if (any(c("dichotomous", "categorical") %in% unlist(type)) && !"variable_level" %in% names(cards)) {
    cli::cli_abort(
      "The {.arg cards} data frame must contain column {.val variable_level} with summary types {.val {c('dichotomous', 'categorical')}}.",
      call = get_cli_abort_call()
    )
  }

  if ("dichotomous" %in% unlist(type)) {
    cards::fill_formula_selectors(
      data[include],
      value =
        cards |>
        dplyr::filter(.data$context %in% "dichotomous") |>
        dplyr::select("variable", "variable_level") |>
        unique() |>
        deframe() |>
        as.list()
    )
  }

  # summary types must be all 'continuous' OR c('categorical', 'dichotomous')
  if (!all(unlist(type) %in% "continuous") && !all(unlist(type) %in% c("categorical", "dichotomous"))) {
    cli::cli_abort(
      "The summary types must all be {.val continuous} or all be {.val {c('categorical', 'dichotomous')}}.",
      call = get_cli_abort_call()
    )
  }
  check_class(statistic, "character")

  # processed arguments are saved into this env
  statistic <- rep_named(include, list(statistic))
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic = statistic,
    include_env = TRUE
  )

  # add the calling env to the statistics
  statistic <- .add_env_to_list_elements(statistic, env = caller_env())

  # save processed function inputs ---------------------------------------------
  tbl_ard_wide_summary_inputs <-
    as.list(environment()) |>
    utils::modifyList(list(data = NULL))
  call <- match.call()

  # check the requested stats are present in ARD data frame
  .check_stats_available(cards = cards, statistic = statistic)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # construct initial tbl_summary object ---------------------------------------
  x <-
    brdg_wide_summary(
      cards = cards,
      variables = include,
      statistic = statistic,
      type = type
    ) |>
    append(
      list(
        cards = list(tbl_ard_wide_summary = cards),
        inputs = tbl_ard_wide_summary_inputs,
        call_list <- list(tbl_ard_wide_summary = call)
      )
    ) |>
    structure(class = c("tbl_ard_wide_summary", "gtsummary"))

  # return tbl_summary table ---------------------------------------------------
  x
}

