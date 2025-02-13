#' Filter Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter hierarchical table rows by frequency row sum.
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   A hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @param t (scalar `numeric`)\cr
#'   Threshold used to determine which rows will be retained.
#' @param gt (scalar `logical`)\cr
#'   Whether to filter for row sums greater than `t` or less than `t`. Default is greater than (`gt = TRUE`).
#' @param eq (scalar `logical`)\cr
#'   Whether to include the value of `t` in the filtered range, i.e. whether to use exclusive comparators (`>`, `<`) or
#'   inclusive comparators (`>=`, `<=`) when filtering. Default is `FALSE`.
#' @param .stat (`string`)\cr
#'   Statistic to use to calculate row sums. This statistic must be present in the table for all hierarchy levels.
#'   Default is `"n"`.
#' @inheritParams rlang::args_dots_empty
#'
#' @return A `gtsummary` of the same class as `x`.
#'
#' @name filter_tbl_hierarchical
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

#' @rdname filter_tbl_hierarchical
#' @export
tbl_filter <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")

  UseMethod("tbl_filter")
}

#' @export
#' @rdname filter_tbl_hierarchical
tbl_filter.tbl_hierarchical <- function(x, filter, ...) {
  set_cli_abort_call()

  ard_args <- attributes(x$cards$tbl_hierarchical)$args
  by_cols <- paste0("group", seq_along(length(ard_args$by)), c("", "_level"))

  browser()
  # add indices to ARD
  x_ard <- x$cards$tbl_hierarchical |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::mutate(idx_sort = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    cards::as_card()

  # re-add dropped args attribute
  attr(x_ard, "args") <- ard_args

  # get `by` variable count rows (do not correspond to a table row)
  rm_idx <- x_ard |>
    dplyr::filter(is.na(group1)) |>
    dplyr::pull("idx_sort") |>
    unique()

  # pull index order (each corresponding to one row of x$table_body)
  pre_sort_idx <- x_ard |>
    dplyr::pull("idx_sort") |>
    unique() |>
    setdiff(rm_idx) |>
    as.character()

  # apply sorting
  x_ard_sort <- x_ard |> cards::ard_filter({{ filter }})

  # pull updated index order after sorting
  post_sort_idx <- x_ard_sort |>
    dplyr::pull("idx_sort") |>
    unique() |>
    setdiff(rm_idx) |>
    as.character()

  # get updated (relative) row positions
  idx <- (seq_len(length(pre_sort_idx)) |> stats::setNames(pre_sort_idx))[post_sort_idx]

  # update x$cards
  x$cards$tbl_hierarchical <- x_ard_sort |> select(-"idx_sort")

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
