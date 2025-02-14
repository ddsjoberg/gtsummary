#' Filter Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter hierarchical table rows.
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   A hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @inheritParams cards::ard_sort
#' @inheritParams rlang::args_dots_empty
#'
#' @details
#' The `filter` argument can be used to filter out rows of a table which do not meet the criteria provided as an
#' expression. Rows can be filtered on the values of any of the possible statistics (`n`, `p`, and `N`) provided they
#' are included at least once in the table, as well as the values of any `by` variables. Filtering is only applied to
#' rows that correspond to the innermost variable in the hierarchy - all outer variable (summary) rows preceding at
#' least one inner row that meets the filtering criteria are kept regardless of whether they meet the filtering criteria
#' themselves. In addition to filtering on individual statistic values, filters can be applied across the row (i.e.
#' across all `by` variable values) by using aggregate functions such as `sum()` and `mean()`.
#'
#' Some examples of possible filters:
#' - `filter = n > 5`
#' - `filter = n == 2 & p < 0.05`
#' - `filter = sum(n) > 4`
#' - `filter = mean(n) > 4 | n > 3`
#' - `filter = any(n > 2 & TRTA == "Xanomeline High Dose")`
#'
#' @return A `gtsummary` of the same class as `x`.
#'
#' @name tbl_filter
#' @seealso [tbl_sort()]
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
#' # Example 1 - Row Sums > 10 ------------------
#' tbl_filter(tbl, sum(n) > 10)
#'
#' # Example 2 - Row Sums <= 5 ------------------
#' tbl_filter(tbl, sum(n) <= 5)
NULL

#' @rdname tbl_filter
#' @export
tbl_filter <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")

  UseMethod("tbl_filter")
}

#' @export
#' @rdname tbl_filter
tbl_filter.tbl_hierarchical <- function(x, filter, ...) {
  set_cli_abort_call()

  ard_args <- attributes(x$cards$tbl_hierarchical)$args
  by_cols <- paste0("group", seq_along(length(ard_args$by)), c("", "_level"))
  x_ard <- x$cards$tbl_hierarchical

  # add dummy rows for variables not in include so their label rows are ordered correctly
  not_incl <- setdiff(ard_args$variables, ard_args$include)
  if (length(not_incl) > 0) {
    x_ard <- x_ard |> mutate(idx_o = seq_len(nrow(x$cards$tbl_hierarchical)))
    for (v in not_incl) {
      i <- length(ard_args$by) + which(ard_args$variables == v)
      x_sum_rows <- x_ard |>
        dplyr::group_by(across(all_of(cards::all_ard_group_n((length(ard_args$by) + 1):i)))) |>
        dplyr::group_map(function(.df, .g) {
          g_cur <- .g[[ncol(.g) - 1]]
          if (!is.na(g_cur) && g_cur == v) {
            # dummy summary row to add in
            .df[1, ] |> mutate(
              variable = g_cur,
              variable_level = .g[[ncol(.g)]],
              stat_name = "no_stat",
              stat = list(0),
              idx_o = min(.df$idx_o) + 1,
              tmp = TRUE
            )
          } else {
            NULL
          }
        }, .keep = TRUE)
      sum_row_pos <- dplyr::bind_rows(x_sum_rows) |> dplyr::pull(idx_o)
      # adjust prior row indices to add in dummy summary rows
      x_ard <- x_ard |>
        dplyr::bind_rows(x_sum_rows) |>
        dplyr::rowwise() |>
        mutate(idx_o = .data$idx_o + sum(sum_row_pos > .data$idx_o)) |>
        dplyr::ungroup()
    }
    x_ard <- x_ard |>
      dplyr::arrange(idx_o, tmp) |>
      select(-"idx_o")
  }

  # add indices to ARD
  x_ard <- x_ard |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::mutate(idx_filter = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  # re-add dropped args attribute
  x_ard <- x_ard |> cards::as_card()
  attr(x_ard, "args") <- ard_args

  # get `by` variable count rows (do not correspond to a table row)
  rm_idx <- x_ard |>
    dplyr::filter(is.na(group1)) |>
    dplyr::pull("idx_filter") |>
    unique()

  # pull index order (each corresponding to one row of x$table_body)
  pre_filter_idx <- x_ard |>
    dplyr::pull("idx_filter") |>
    unique() |>
    setdiff(rm_idx) |>
    as.character()

  browser()
  # apply filtering
  x_ard_filter <- x_ard |> cards::ard_filter({{ filter }})

  # pull updated index order after filtering
  post_filter_idx <- x_ard_filter |>
    dplyr::pull("idx_filter") |>
    unique() |>
    setdiff(rm_idx) |>
    as.character()

  # get updated (relative) row positions
  idx <- (seq_len(length(pre_filter_idx)) |> stats::setNames(pre_filter_idx))[post_filter_idx]

  # update x$cards
  if ("tmp" %in% names(x_ard_filter)) {
    x_ard_filter <- x_ard_filter |>
      dplyr::filter(is.na(tmp)) |>
      select(-"tmp")
  }
  x$cards$tbl_hierarchical <- x_ard_filter |> select(-"idx_filter")

  # update x$table_body
  x$table_body <- x$table_body[idx, ]

  x
  # if (nrow(x$table_body) > 0) {
  #     cli::cli_inform(
  #       "For readability, all summary rows preceding at least one row that meets the filtering criteria are kept
  #       regardless of whether they meet the filtering criteria themselves.",
  #       .frequency = "once",
  #       .frequency_id = "sum_rows_lt"
  #     )
  #   }
}
