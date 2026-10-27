#' Update Header Statistics
#'
#' @description
#' `r lifecycle::badge('experimental')`\cr
#' Use this function to update the statistics that can be injected into
#' gtsummary table headers with `modify_header()`, e.g. `{N}`, `{n}`, `{p}`, and `{level}`.
#' This is useful, for example, when the headers should report population Ns
#' that differ from the Ns in the data summarized in the table.
#'
#' - **Tables with a `by` variable** (columns `'stat_1'`, `'stat_2'`, etc.):
#'   pass a univariate tabulation of the `by` variable, e.g. `cards::ard_tabulate(variables = trt)`.
#'   The levels of the tabulated variable are matched _by position_ to the
#'   `'stat_1'`, `'stat_2'`, etc. columns.
#'   If the table also includes an overall `'stat_0'` column (e.g. from `add_overall()`),
#'   its statistics are updated with the total N from the tabulation.
#' - **Tables without a `by` variable** (a single `'stat_0'` column):
#'   pass the total N, e.g. `cards::ard_total_n()`.
#'
#' Statistics are glued into the headers when `modify_header()` is called.
#' Therefore, after updating the statistics, you must call `modify_header()`
#' to see the updated numbers in the table.
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param card (`card`)\cr
#'   Object of class 'card'. Either a univariate tabulation, typically created
#'   with `cards::ard_tabulate()`, or the total N, typically created with
#'   `cards::ard_total_n()`. See details above.
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
#'
#' # Example 2 ----------------------------------
#' tbl_summary(trial, include = age) |>
#'   update_header_stats(
#'     card = cards::ard_total_n(rbind(trial, trial))
#'   ) |>
#'   modify_header(stat_0 ~ "**Overall**  \n N = {N}")
update_header_stats <- function(x, card) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(update_header_stats = match.call()))

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(card)
  check_class(x, "gtsummary")
  check_class(card, "card")

  # check the ARD doesn't have grouping columns
  card_group_colnames <- dplyr::select(card, cards::all_ard_groups()) |> names()
  if (!is_empty(card_group_colnames)) {
    cli::cli_abort(
      "The {.arg card} argument object cannot include {.emph group} columns, i.e. {.val {card_group_colnames}}.",
      call = get_cli_abort_call()
    )
  }

  # check there is only one variable in the ARD
  if (!"variable" %in% names(card) || length(unique(card$variable)) != 1L) {
    cli::cli_abort(
      "The {.arg card} argument object must contain results for a single variable.",
      call = get_cli_abort_call()
    )
  }

  # check the gtsummary table includes stat columns
  stat_colnames <- dplyr::select(x$table_body, all_stat_cols()) |> names()
  by_stat_colnames <- setdiff(stat_colnames, "stat_0")
  if (is_empty(stat_colnames) ||
      !setequal(by_stat_colnames, paste0("stat_", seq_along(by_stat_colnames), recycle0 = TRUE))) {
    cli::cli_abort(
      c("The {.arg x} table must contain an overall {.val stat_0} column and/or sequential
         stat columns, e.g. {.val {paste0('stat_', seq_len(3))}}.",
        i = "Run {.fun show_header_names} to see column names."),
      call = get_cli_abort_call()
    )
  }

  # update the statistics ------------------------------------------------------
  if (is_empty(by_stat_colnames)) {
    x <- .update_header_stats_overall(x, card)
  } else {
    x <- .update_header_stats_by(x, card, by_stat_colnames, has_overall = "stat_0" %in% stat_colnames)
  }

  # return updated table -------------------------------------------------------
  cli::cli_inform(
    c("!" = "You must update the headers to utilize the updated numbers.",
      "i" = "For example, {.code modify_header(all_stat_cols() ~ '**{{level}}**, N = {{n}}')}")
  )
  x$call_list <- updated_call_list
  x
}

# update stats for tables without a `by` variable, i.e. only a `stat_0` column
.update_header_stats_overall <- function(x, card) {
  if ("variable_level" %in% names(card) || !"N" %in% card$stat_name) {
    cli::cli_abort(
      c("For tables without a {.arg by} variable (i.e. a single {.val stat_0} column),
         the {.arg card} argument must contain the total N.",
        i = "This object is typically created with {.fun cards::ard_total_n}."),
      call = get_cli_abort_call()
    )
  }

  lst_total_n <- cards::get_ard_statistics(card)
  header <- x$table_styling$header
  all_rows <- rep_len(TRUE, nrow(header))
  header <- .assign_header_stats(header, all_rows, c(N = lst_total_n[["N"]], n = lst_total_n[["N"]], p = 1))
  if ("N_unweighted" %in% names(lst_total_n)) {
    header <-
      .assign_header_stats(
        header, all_rows,
        c(N_unweighted = lst_total_n[["N_unweighted"]], n_unweighted = lst_total_n[["N_unweighted"]], p_unweighted = 1)
      )
  }

  x$table_styling$header <- header
  x
}

# update stats for tables with a `by` variable, i.e. `stat_1`, `stat_2`, etc. columns
.update_header_stats_by <- function(x, card, by_stat_colnames, has_overall) {
  if (!"variable_level" %in% names(card)) {
    cli::cli_abort(
      c("For tables with a {.arg by} variable (i.e. columns {.val {by_stat_colnames}}),
         the {.arg card} argument must be a tabulation of a single variable.",
        i = "This object is typically created with {.fun cards::ard_tabulate}."),
      call = get_cli_abort_call()
    )
  }

  # check the number of `stat_##` columns matches the number of levels of the variable
  levels <- unique(unlist(card$variable_level))
  if (length(levels) != length(by_stat_colnames)) {
    cli::cli_abort(
      "The number of levels in the {.arg card} object ({.val {levels}})
       {.emph must} match the number of statistic columns {.val {by_stat_colnames}}.",
      call = get_cli_abort_call()
    )
  }

  # transform the ARD to one line per variable level ---------------------------
  df_header_stats <-
    card |>
    dplyr::filter(.data$stat_name %in% c("N", "n", "p", "N_unweighted", "n_unweighted", "p_unweighted")) |>
    dplyr::select(level = "variable_level", "stat_name", "stat") |>
    dplyr::mutate(level = unlist(.data$level) |> as.character()) |>
    tidyr::pivot_wider(
      id_cols = "level",
      names_from = "stat_name",
      values_from = "stat",
      values_fn = unlist
    ) |>
    dplyr::mutate(column = paste0("stat_", dplyr::row_number()), .before = 1L)

  # update the header ----------------------------------------------------------
  header <- x$table_styling$header
  idx <- match(df_header_stats$column, header$column)
  for (stat in setdiff(names(df_header_stats), "column")) {
    header <- .assign_header_stats(header, idx, stats::setNames(list(df_header_stats[[stat]]), stat))
  }

  # the total N applies to every column of the table
  for (N in intersect(c("N", "N_unweighted"), names(df_header_stats))) {
    total_n <- df_header_stats[[N]][1]
    header <- .assign_header_stats(header, rep_len(TRUE, nrow(header)), stats::setNames(list(total_n), N))

    # update the overall column's stats
    if (has_overall) {
      header <-
        .assign_header_stats(
          header, header$column %in% "stat_0",
          stats::setNames(list(total_n, 1), paste0(c("n", "p"), sub("^N", "", N)))
        )
    }
  }

  x$table_styling$header <- header
  x
}

# assign stats to the `modify_stat_*` columns of the header in rows `i`
.assign_header_stats <- function(header, i, stats) {
  for (stat in names(stats)) {
    col <- paste0("modify_stat_", stat)
    if (!col %in% names(header)) {
      header[[col]] <- vctrs::vec_init(stats[[stat]], n = nrow(header))
    }
    header[[col]][i] <- stats[[stat]]
  }
  header
}
