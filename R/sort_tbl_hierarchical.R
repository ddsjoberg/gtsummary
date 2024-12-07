#' Sort Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to sort hierarchical tables. Options for sorting criteria are:
#'
#' 1. Frequency - within each section of the hierarchy table, frequency sums are calculated for each row and rows are
#'    ordered accordingly (default).
#' 2. Alphanumeric - rows are ordered alphanumerically by label text. By default, [tbl_hierarchical()] sorts tables
#'    in ascending alphanumeric order (i.e. A to Z).
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   A hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @param sort (`string`)\cr
#'   Specifies sorting to perform. Values must be one of `c("frequency", "alphanumeric")`. Default is `"frequency"`.
#' @param desc (scalar `logical`)\cr
#'   Whether to sort rows in ascending or descending order. Default is descending (`desc = TRUE`).
#' @param .stat (`string`)\cr
#'   Statistic to use to calculate row sums when `sort = "frequency"`. This statistic must be present in the table for
#'   all hierarchy levels. Default is `"n"`.
#' @inheritParams rlang::args_dots_empty
#'
#' @return A `gtsummary` of the same class as `x`.
#'
#' @name sort_tbl_hierarchical
#' @seealso [tbl_filter()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(AETERM %in% unique(cards::ADAE$AETERM)[1:5])
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
#' # Example 1 - Decreasing Frequency Sort ------------------
#' tbl_sort(tbl)
#'
#' # Example 2 - Ascending Alphanumeric Sort (Z to A) -------
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
tbl_sort.tbl_hierarchical <- function(x, sort = "frequency", desc = TRUE, .stat = "n", ...) {
  set_cli_abort_call()

  # process and check inputs ----------------------------------------------------------------------
  check_scalar_logical(desc)
  check_string(.stat)

  if (!sort %in% c("frequency", "alphanumeric")) {
    cli::cli_abort(
      "The {.arg sort} argument must be either {.val frequency} or {.val alphanumeric}.",
      call = get_cli_abort_call()
    )
  }

  overall <- "..ard_hierarchical_overall.." %in% x$table_body$variable

  outer_cols <- sapply(
    x$table_body |> select(cards::all_ard_groups("names")),
    function(x) dplyr::last(unique(stats::na.omit(x)))
  )
  inner_col <- setdiff(
    x$table_body$variable,
    x$table_body |> select(cards::all_ard_groups("names")) |> unlist() |> unique()
  )

  if (sort == "alphanumeric") {
    # summary rows remain at the top of each sub-section
    rep_str <- if (desc) "zzzz" else " "

    # overall row always appears first
    if (desc && "..ard_hierarchical_overall.." %in% x$table_body$variable) {
      ovrl_row <- x$table_body[1, ]
      x$table_body <- x$table_body[-1, ]
    }

    # sort by label -------------------------------------------------------------------------------
    sort_cols <- c(x$table_body |> select(cards::all_ard_groups("levels")) |> names(), "inner_var", "label")

    x$table_body <- x$table_body |>
      dplyr::rowwise() |>
      dplyr::mutate(inner_var = if (!.data$variable == inner_col) rep_str else .data$variable) |>
      dplyr::ungroup() |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ tidyr::replace_na(., rep_str))) |>
      dplyr::arrange(across(sort_cols, ~ if (desc) dplyr::desc(.x) else .x)) |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ str_replace(., paste0("^", rep_str, "$"), NA))) |>
      select(-"inner_var")

    if (desc) x$table_body <- dplyr::bind_rows(ovrl_row, x$table_body)
  } else {
    # get row sums --------------------------------------------------------------------------------
    x <- .append_hierarchy_row_sums(x, .stat)

    # append outer hierarchy level sums in each row to sort at all levels -------------------------
    for (g in names(outer_cols)) {
      x$table_body <- x$table_body |> dplyr::group_by(across(c(g, paste0(g, "_level"))), .add = TRUE)
      x$table_body <- x$table_body |>
        dplyr::left_join(
          x$table_body |>
            dplyr::summarize(!!paste0("sum_", g) := dplyr::first(.data$sum_row)),
          by = x$table_body |> dplyr::group_vars()
        )
    }

    # summary rows remain at the top of each sub-section
    x$table_body <- x$table_body |>
      dplyr::ungroup() |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ tidyr::replace_na(., " "))) |>
      dplyr::rowwise() |>
      dplyr::mutate(inner_var = if (!.data$variable == inner_col) " " else .data$variable) |>
      dplyr::ungroup()

    # sort by row sum -----------------------------------------------------------------------------
    sort_cols <- c(rbind(
      x$table_body |> select(cards::all_ard_groups("names")) |> names(),
      x$table_body |> select(starts_with("sum_group")) |> names(),
      x$table_body |> select(cards::all_ard_groups("levels")) |> names()
    ), "inner_var", "sum_row", "label")

    x$table_body <- x$table_body |>
      dplyr::arrange(across(sort_cols, ~ if (is.numeric(.x) && desc) dplyr::desc(.x) else .x)) |>
      dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ str_replace(., "^ $", NA))) |>
      select(-starts_with("sum_"), -"inner_var")
  }

  x
}

.append_hierarchy_row_sums <- function(x, .stat) {
  cards <- x$cards$tbl_hierarchical

  if (!.stat %in% cards$stat_name) {
    cli::cli_abort(
      "The {.arg .stat} argument is {.val {(.stat)}} but this statistic is not present in {.arg x}. For all valid
      statistic options see the {.val stat_name} column of {.code x$cards$tbl_hierarchical}.",
      call = get_cli_abort_call()
    )
  }

  by_cols <- if (ncol(x$table_body |> select(starts_with("stat_"))) > 1) c("group1", "group1_level") else NA
  outer_cols <- sapply(
    x$table_body |> select(cards::all_ard_groups("names")),
    function(x) dplyr::last(unique(stats::na.omit(x)))
  )

  # update logical variable_level entries from overall row to character
  cards$variable_level[cards$variable == "..ard_hierarchical_overall.."] <- x$table_body |>
    dplyr::filter(.data$variable == "..ard_hierarchical_overall..") |>
    dplyr::pull("label") |>
    as.list()

  # extract row sums ------------------------------------------------------------------------------
  cards <- cards |>
    dplyr::filter(.data$stat_name == .stat, .data$variable %in% x$table_body$variable) |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -by_cols))) |>
    dplyr::summarise(sum_row = sum(unlist(.data$stat))) |>
    dplyr::ungroup() |>
    dplyr::rename(label = "variable_level") |>
    tidyr::unnest(cols = everything())

  # match cards names to x$table_body -------------------------------------------------------------
  if (length(by_cols) > 1) {
    names(cards)[grep("group", names(cards))] <- x$table_body |>
      select(cards::all_ard_groups()) |>
      names()
  }
  cards[cards$variable == "..ard_hierarchical_overall..", 1] <- "..ard_hierarchical_overall.."

  # fill in NAs to align cards layout with x$table_body -------------------------------------------
  cards <- cards |>
    dplyr::rowwise() |>
    dplyr::mutate(across(
      cards::all_ard_groups(),
      ~ if (is.na(.x) && !grepl("_level", dplyr::cur_column()) && .data$variable == outer_cols[dplyr::cur_column()]) {
        .data$variable
      } else if (is.na(.x) && .data$variable %in% outer_cols[gsub("_level", "", dplyr::cur_column())]) {
        .data$label
      } else {
        .x
      }
    ))

  # for any variables not in include, calculate group sums ----------------------------------------
  if (!all(outer_cols %in% cards$variable)) {
    gp_vars <- outer_cols[outer_cols %in% setdiff(outer_cols, cards$variable)]
    gp_cols <- names(gp_vars)

    cli::cli_inform(
      "Not all hierarchy variables present in the table were included in the {.arg include} argument.
      These variables ({gp_vars}) do not have event rate data available so the total sum of the event
      rates for this hierarchy section will be used instead. To use event rates for all sections of the table,
      set {.code include = everything()} when creating your table via {.fun tbl_hierarchical}."
    )

    for (i in seq_along(gp_cols)) {
      cards <- cards |>
        dplyr::bind_rows(
          cards |>
            dplyr::filter(.data$variable != "..ard_hierarchical_overall..") |>
            dplyr::group_by(across(c(gp_cols[1:i], paste0(gp_cols[1:i], "_level")))) |>
            dplyr::summarize(sum_row = sum(.data$sum_row)) |>
            dplyr::mutate(
              variable = .data[[gp_cols[i]]],
              label = .data[[paste0(gp_cols[i], "_level")]]
            )
        )
    }
  }

  # append row sums to x$table_body ---------------------------------------------------------------
  x$table_body <- x$table_body |>
    dplyr::left_join(
      cards,
      by = c(cards |> select(-"sum_row") |> names())
    )

  x
}
