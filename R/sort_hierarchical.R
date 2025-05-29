#' Sort Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to sort hierarchical tables. Options for sorting criteria are:
#'
#' 1. Descending - within each section of the hierarchy table, event rate sums are calculated for each row and rows are
#'    sorted in descending order by sum (default).
#' 2. Alphanumeric - rows are ordered alphanumerically (i.e. A to Z) by label text. By default, [tbl_hierarchical()]
#'    sorts tables in alphanumeric order.
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   a hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @param sort (`string`)\cr
#'   type of sorting to perform. Value must be one of:
#'   - `"alphanumeric"` - at each hierarchy level of the table, rows are ordered alphanumerically (i.e. A to Z)
#'     by label text.
#'   - `"descending"` - at each hierarchy level of the table, count sums are calculated for each row and rows are
#'     sorted in descending order by sum. If `sort = "descending"`, the `n` statistic is used to calculate row sums if
#'     included in `statistic` for all variables, otherwise `p` is used. If neither `n` nor `p` are present in `x` for
#'     all variables, an error will occur.
#'
#'   Defaults to `"descending"`.
#'
#' @return A `gtsummary` of the same class as `x`.
#'
#' @seealso [filter_hierarchical()]
#'
#' @export
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' theme_gtsummary_compact()
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(AEBODSYS %in% c("SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
#'                                 "EAR AND LABYRINTH DISORDERS")) |>
#'   dplyr::filter(.by = AEBODSYS, dplyr::row_number() < 20)
#'
#' tbl <-
#'   tbl_hierarchical(
#'     data = ADAE_subset,
#'     variables = c(AEBODSYS, AEDECOD),
#'     by = TRTA,
#'     denominator = cards::ADSL |> mutate(TRTA = ARM),
#'     id = USUBJID,
#'     overall_row = TRUE
#'   ) |>
#'   add_overall()
#'
#'
#' # Example 1 - Descending Frequency Sort ------------------
#' sort_hierarchical(tbl)
#'
#' # Example 2 - Alphanumeric Sort --------------------------
#' sort_hierarchical(tbl, sort = "alphanumeric")
#'
#' reset_gtsummary_theme()
sort_hierarchical <- function(x, sort = c("descending", "alphanumeric")) {
  set_cli_abort_call()

  # check input
  check_not_missing(x)
  check_class(x, "gtsummary")

  sort <- arg_match(sort, error_call = get_cli_abort_call())
  ard_args <- attributes(x$cards$tbl_hierarchical)$args
  x_ard <- x$cards$tbl_hierarchical

  # add row indices match structure of ard to x$table_body
  reshape_x <- .reshape_ard_compare(x, x_ard, ard_args, sort)
  x <- reshape_x$x
  x_ard <- reshape_x$x_ard

  # get `by` variable count rows (do not correspond to a table row)
  rm_idx <- x_ard |>
    dplyr::filter(if (!is_empty(ard_args$by)) is.na(.data$group1) else context != "hierarchical") |>
    dplyr::pull("pre_idx") |>
    unique()

  # apply sorting
  x_ard_sort <- x_ard |> cards::sort_ard_hierarchical(sort)

  # pull updated index order after sorting
  idx_sort <- x_ard_sort |>
    dplyr::pull("pre_idx") |>
    unique() |>
    setdiff(rm_idx)

  if ("tmp" %in% names(x_ard_sort)) {
    x_ard_sort <- x_ard_sort |>
      dplyr::filter(is.na(.data$tmp)) |>
      select(-"tmp")
  }

  # if overall column present, filter x$cards$add_overall
  if ("add_overall" %in% names(x$cards)) {
    # update x$cards$add_overall
    x$cards$add_overall <- x$cards$add_overall |> cards::sort_ard_hierarchical(sort)
  }

  # update x$cards$tbl_hierarchical
  x$cards$tbl_hierarchical <- x_ard_sort |> select(-"pre_idx")

  # update x$table_body
  x$table_body <- x$table_body[match(idx_sort, x$table_body$pre_idx), ] |> select(-"pre_idx")

  x
}

.reshape_ard_compare <- function(x, x_ard, ard_args, sort = NULL) {
  by_cols <- if (length(ard_args$by) > 0) c("group1", "group1_level") else NULL

  # add dummy rows for variables not in include so their label rows are sorted correctly
  x_ard <- x_ard |> .append_not_incl(ard_args, sort)

  # add indices to ARD
  x_ard <- x_ard |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::mutate(pre_idx = dplyr::cur_group_id())

  # get grouping structure
  gps <- x_ard |>
    dplyr::group_keys() |>
    dplyr::mutate(pre_idx = dplyr::row_number()) |>
    cards::as_card()

  # if by variable present, shift grouping columns
  if (!is_empty(by_cols)) gps <- gps |> cards::rename_ard_groups_shift(shift = -1)

  gps <- gps |>
    dplyr::filter(!.data$variable %in% ard_args$by) |>
    dplyr::rename(label = "variable_level")

  # match overall row if present
  overall_lbl <- x$table_body$label[x$table_body$variable == "..ard_hierarchical_overall.."]
  if (length(overall_lbl) > 0) {
    gps$label[gps$variable == "..ard_hierarchical_overall.."] <- overall_lbl
    if (length(ard_args$variables) > 1) {
      gps$group1[gps$variable == "..ard_hierarchical_overall.."] <- "..ard_hierarchical_overall.."
    }
  }

  # match structure of ARD grouping columns to x$table_body grouping columns
  gps <- gps |> tidyr::unnest(everything())
  outer_cols <- if (length(ard_args$variables) > 1) {
    ard_args$variables |>
      utils::head(-1) |>
      stats::setNames(paste0("group", seq_len(length(ard_args$variables) - 1)))
  } else {
    NULL
  }
  for (g in names(outer_cols)) {
    which_g <- gps$variable == outer_cols[g]
    gps[g][which_g, ] <- gps$variable[which_g]
    gps[paste0(g, "_level")][which_g, ] <- gps$label[which_g]
  }
  x$table_body <- x$table_body |> dplyr::left_join(gps, by = names(gps) |> utils::head(-1))

  # re-add dropped args attribute
  x_ard <- x_ard |>
    dplyr::ungroup() |>
    cards::as_card()
  attr(x_ard, "args") <- ard_args

  list(x = x, x_ard = x_ard)
}

.append_not_incl <- function(x, ard_args, sort = NULL) {
  # add dummy rows for variables not in include so their label rows are sorted correctly
  not_incl <- setdiff(ard_args$variables, ard_args$include)
  if (length(not_incl) > 0) {
    cli::cli_inform(
      "Not all hierarchy variables present in the table were included in the {.arg include} argument.
      These variables ({not_incl}) do not have event rate data available so the total sum of the event rates
      for this hierarchy section will be used instead. To use true event rates for all sections of the table,
      set {.code include = everything()} when creating your table via {.fun tbl_hierarchical}."
    )

    for (v in not_incl) {
      i <- length(ard_args$by) + which(ard_args$variables == v)
      x_sum_rows <- x |>
        dplyr::group_by(across(all_of(cards::all_ard_group_n((length(ard_args$by) + 1):i)))) |>
        dplyr::group_map(function(.df, .g) {
          # get pseudo-summary row stat value for descending sort
          if (!is.null(sort) && sort == "descending") {
            stat_nm <- setdiff(.df$stat_name, "N")[1]
            sum <- .df |>
              dplyr::filter(.data$stat_name == !!stat_nm) |>
              dplyr::summarize(sum_stat = sum(unlist(.data$stat))) |>
              dplyr::pull("sum_stat")
          }
          g_cur <- .g[[ncol(.g) - 1]]
          if (!is.na(g_cur) && g_cur == v) {
            # dummy summary row to add in
            .df[1, ] |> mutate(
              variable = g_cur,
              variable_level = .g[[ncol(.g)]],
              stat_name = if (!is.null(sort) && sort == "descending") stat_nm else "no_stat",
              stat = if (!is.null(sort) && sort == "descending") list(sum) else list(0),
              tmp = TRUE
            )
          } else {
            NULL
          }
        }, .keep = TRUE)

      x <- x |> dplyr::bind_rows(x_sum_rows)
    }
  }

  x
}
