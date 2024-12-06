#' Sort Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to sort hierarchical tables. Options for sorting criteria are:
#'
#' 1. Alphanumeric - rows are ordered alphanumerically by label text (default).
#' 2. Frequency - within each section of the hierarchy table, frequency sums are calculated for each row and rows are
#'    ordered accordingly.
#'
#' @param x (`tbl_hierarchical`)\cr
#'   A hierarchical gtsummary table of class `'tbl_hierarchical'`.
#' @param sort (`string`)\cr
#'   Specifies sorting to perform. Values must be one of `c("alphanumeric", "frequency")`. Default is `"frequency"`.
#' @param desc (scalar `logical`)\cr
#'   Whether to sort rows in ascending or descending order. Default is descending (`desc = TRUE`).
#' @param .stat (`string`)\cr
#'   Statistic to use to calculate row sums when `sort = "frequency"`. This statistic must be present in the table for
#'   all hierarchy levels.
#'
#' @name sort_tbl_hierarchical
#'
#' @examples
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:5]
#'   )
#'
#' tbl <- tbl_hierarchical(
#'   data = ADAE_subset,
#'   variables = c(SEX, RACE, AETERM),
#'   by = TRTA,
#'   denominator = cards::ADSL |> mutate(TRTA = ARM),
#'   id = USUBJID,
#'   overall_row = TRUE
#' )
#'
#' # Example 1 - Decreasing Frequency Sort ------
#' tbl_sort(tbl)
#'
#' # Example 2 - Reverse Alphanumeric Sort ------
#' tbl_sort(tbl, sort = "alphanumeric")
NULL

#' @rdname sort_tbl_hierarchical
#' @export
tbl_sort <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")

  UseMethod("tbl_sort")
}

#' @rdname sort_tbl_hierarchical
#' @export
tbl_sort.tbl_hierarchical <- function(x, sort = "frequency", desc = TRUE, .stat = "n") {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_scalar_logical(desc)
  check_string(.stat)

  if (!sort %in% c("frequency", "alphanumeric")) {
    cli::cli_abort(
      "The {.arg sort} argument must be either {.val frequency} or {.val alphanumeric}.",
      call = get_cli_abort_call()
    )
  }

  u_cols <- x$table_body |> select(cards::all_ard_groups("names")) |> unlist() |> unique()

  if (sort == "alphanumeric") {
    sort_cols <- c(x$table_body |> select(cards::all_ard_groups("levels")) |> names(), "inner_var", "label")
    rep_str <- if (desc) "zzzz" else " "

    # overall row always appears first
    if (desc && "..ard_hierarchical_overall.." %in% x$table_body$variable) {
      ovrl_row <- x$table_body[1, ]
      x$table_body <- x$table_body[-1,]
    }

    x$table_body <- x$table_body |>
      dplyr::rowwise() |>
      dplyr::mutate(inner_var = if (.data$variable %in% u_cols) rep_str else .data$variable) |>
      dplyr::ungroup() |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~tidyr::replace_na(., rep_str))) |>
      dplyr::arrange(across(sort_cols, ~ if (desc) dplyr::desc(.x) else .x)) |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~str_replace(., paste0("^", rep_str, "$"), NA))) |>
      select(-"inner_var")

    if (desc) x$table_body <- dplyr::bind_rows(ovrl_row, x$table_body)
  } else {
    x <- .append_hierarchy_row_sums(x, .stat)
    g_cols <- sapply(x$table_body |> select(cards::all_ard_groups("names")), function(x) tail(unique(na.omit(x)), 1))

    # sort summary rows first
    x$table_body <- x$table_body |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~tidyr::replace_na(., " ")))

    # assign counts for each hierarchy level
    for (g in names(g_cols)) {
      x$table_body <- x$table_body |>
        dplyr::group_by(across(c(g, paste0(g, "_level"))), .add = TRUE)

      x$table_body <- x$table_body |>
        dplyr::left_join(
          dplyr::summarize(x$table_body, !!paste0("count_", g) := dplyr::first(count_total)),
          by = x$table_body |> dplyr::group_vars()
        )
    }
    x$table_body <- x$table_body |>
      dplyr::rowwise() |>
      dplyr::mutate(inner_var = if (.data$variable %in% u_cols) " " else .data$variable) |>
      dplyr::ungroup()

    # sort by counts ------------------------------------------------------------------------------
    sort_cols <- c(rbind(
      x$table_body |> select(cards::all_ard_groups("names")) |> names(),
      x$table_body |> select(starts_with("count_group")) |> names(),
      x$table_body |> select(cards::all_ard_groups("levels")) |> names()
    ), "inner_var", "count_total", "label")

    x$table_body <- x$table_body |>
      dplyr::arrange(across(sort_cols, ~ if (is.numeric(.x) && desc) dplyr::desc(.x) else .x)) |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~str_replace(., "^ $", NA))) |>
      dplyr::select(-starts_with("count_"), -"inner_var")
  }

  x
}

.append_hierarchy_row_sums <- function(x, .stat) {
  if (!.stat %in% x$cards$tbl_hierarchical$stat_name) {
    cli::cli_abort(
      "The {.arg .stat} argument is {.val {(.stat)}} but this statistic is not present in {.arg x}. For all valid
      statistic options see the {.val stat_name} column of {.code x$cards$tbl_hierarchical}.",
      call = get_cli_abort_call()
    )
  }

  cards <- x$cards$tbl_hierarchical
  by_cols <- if (ncol(x$table_body |> select(starts_with("stat_"))) > 1) c("group1", "group1_level") else NA
  g_cols <- sapply(x$table_body |> select(cards::all_ard_groups("names")), function(x) tail(unique(na.omit(x)), 1))

  # fill in variable_level column of cards
  cards$variable_level[cards$variable == "..ard_hierarchical_overall.."] <- x$table_body |>
    dplyr::filter(variable == "..ard_hierarchical_overall..") |>
    dplyr::pull("label") |>
    as.list()

  # extract counts ------------------------------------------------------------------------------
  cards <- cards |>
    dplyr::filter(stat_name == .stat, variable %in% x$table_body$variable) |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -by_cols))) |>
    dplyr::summarise(count_total = sum(unlist(stat))) |>
    dplyr::ungroup() |>
    dplyr::rename(label = variable_level) |>
    tidyr::unnest(cols = everything())

  # match names to x$table_body
  if (length(by_cols) > 1) {
    names(cards)[grep("group", names(cards))] <- x$table_body |>
      select(cards::all_ard_groups()) |>
      names()
  }
  cards[cards$variable == "..ard_hierarchical_overall..", 1] <- "..ard_hierarchical_overall.."

  # align cards layout with x$table_body --------------------------------------------------------
  cards <- cards |>
    dplyr::rowwise() |>
    dplyr::mutate(across(
      cards::all_ard_groups(),
      ~ if (is.na(.x) && !grepl("_level", dplyr::cur_column()) && variable == g_cols[dplyr::cur_column()]) {
        variable
      } else if (is.na(.x) && variable %in% g_cols[gsub("_level", "", dplyr::cur_column())]) {
        label
      } else {
        .x
      }
    ))

  # calculate total group sums for any variables not in include ---------------------------------
  if (!all(g_cols %in% cards$variable)) {
    gp_vars <- g_cols[g_cols %in% setdiff(g_cols, cards$variable)]
    cli::cli_inform(
      "Not all hierarchy variables present in the table were included in the {.arg include} argument.
      These variables ({gp_vars}) do not have event rate data available so the total sum of the event
      rates for this hierarchy section will be used instead. To use event rates for all sections of the table,
      set {.code include = everything()} when creating your table via {.fun tbl_hierarchical}."
    )

    gp_cols <- names(gp_vars)
    for (i in seq_along(gp_cols)) {
      cards <- cards |>
        dplyr::bind_rows(
          cards |>
            dplyr::filter(variable != "..ard_hierarchical_overall..") |>
            dplyr::group_by(across(c(gp_cols[1:i], paste0(gp_cols[1:i], "_level")))) |>
            dplyr::summarize(count_total = sum(count_total)) |>
            dplyr::mutate(
              variable = .data[[gp_cols[i]]],
              label = .data[[paste0(gp_cols[i], "_level")]]
            )
        )
    }
  }

  # add counts to x$table_body ------------------------------------------------------------------
  x$table_body <- x$table_body |>
    dplyr::left_join(
      cards,
      by = c(cards |> select(-"count_total") |> names())
    )

  x
}
