#' Update Header Statistics
#'
#' Use this function to update the statistics that can be injected into
#' gtsummary table headers.
#' Tables must have underlying columns statistic columns (e.g. 'stat_1', 'stat_2'); typical in tables
#' generated from `tbl_summary(by)`.
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param card (`card`)\cr
#'   Object of class 'card', typically created with `cards::ard_tabulate()`.
#'   The ARD must be a univariate tabulation.
#'
#' @return gtsummary object
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' tbl_summary(trial, by = trt, include = age) |>
#'   update_header_stats(
#'     # doubling the trial data set and the Ns
#'     card =
#'       rbind(trial, trial) |>
#'       cards::ard_tabulate(variables = trt)
#'   ) |>
#'   modify_header(all_stat_cols() ~ "**{level}**  \n N = {n}")
update_header_stats <- function(x, card) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  check_class(card, "card")

  # check the ARD doesn't have grouping columns
  card_group_colnames <- select(card, cards::all_ard_groups()) |> names()
  if (!rlang::is_empty(card_group_colnames)) {
    cli::cli_abort(
      "The {.arg card} argument object cannot include {.emph group} columns, i.e. {.val {card_group_colnames}}.",
      call = get_cli_abort_call()
    )
  }

  # check there are 'variable' and `variable_level` columns
  if (!all(c("variable_level", "variable") %in% names(card))) {
    cli::cli_abort(
      c("The {.arg card} argument object must include columns {.val {c('variable', 'variable_level')}}.",
        i = "This object is typically created with {.fun cards::ard_tabulate}."),
      call = get_cli_abort_call()
    )
  }

  # check there is only one variable tabulated in the ARD
  if (length(unique(card$variable)) > 1L) {
    cli::cli_abort(
      "The ARD may contain results for a single variable only. Not {.val {unique(card$variable)}}.",
      call = get_cli_abort_call()
    )
  }

  # check the gtsummary table includes columns stat_1, stat_2, etc.
  stat_colnames <- select(x$table_body, all_stat_cols()) |> names()
  if (!setequal(stat_colnames, paste0("stat_", seq_along(stat_colnames))) || rlang::is_empty(stat_colnames)) {
    cli::cli_abort(
      c("The {.arg gtsummary} table must contain sequential stat columns, e.g. {.val {paste0('stat_', seq_len(3))}}.",
        i = "Run {.fun show_header_names} to see column names."),
      call = get_cli_abort_call()
    )
  }

  # check the number of `stat_##` columns matches the number of levels of the variable
  if (length(unique(unlist(card$variable_level))) != length(stat_colnames)) {
    cli::cli_abort(
      "The number of levels in the {.arg card} object ({.val {unique(unlist(card$variable_level))}})
       {.emph must} match the number of statistic columns {.val {stat_colnames}}.",
      call = get_cli_abort_call()
    )
  }

  # transform the ARD to one line per variable level ---------------------------
  df_header_stats <-
    card |>
    select(level = "variable_level", "stat_name", "stat") |>
    mutate(level = unlist(.data$level) |> as.character()) |>
    tidyr::pivot_wider(
      id_cols = "level",
      names_from = "stat_name",
      values_from = "stat",
      values_fn = unlist
    ) |>
    mutate(column = paste0("stat_", dplyr::row_number()), .before = 1L) |>
    dplyr::rename_with(.fn = ~paste0("modify_stat_", .), .cols = -"column")

  # update the gtsummary object ------------------------------------------------
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::rows_update(
      df_header_stats,
      by = "column",
      unmatched = "ignore"
    )

  # return updated table -------------------------------------------------------
  x |>
    modify_table_body(~select(.x, all_stat_cols())) |>
    show_header_names()
  cat("\n")
  cli::cli_inform(
    c("!" = "You must update the headers to utilize the updated numbers.",
      "i" = "For example, {.code modify_header(all_stat_cols() ~ '**{{level}}**, N = {{n}}')}")
  )
  x
}
