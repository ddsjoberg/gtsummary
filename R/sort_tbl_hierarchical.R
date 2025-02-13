#' Sort Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to sort hierarchical tables. Options for sorting criteria are:
#'
#' 1. Descending - within each section of the hierarchy table, count sums are calculated for each row and rows are
#'    sorted in descending order by sum (default).
#' 2. Alphanumeric - rows are ordered alphanumerically by label text. By default, [tbl_hierarchical()] sorts tables
#'    in ascending alphanumeric order (i.e. A to Z).
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   a hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @inheritParams cards::ard_sort
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
#' # Example 1 - Descending Frequency Sort ------------------
#' tbl <- tbl_sort(tbl, sort = "descending")
#' tbl
#'
#' # Example 2 - Alphanumeric Sort --------------------------
#' tbl <- tbl_sort(tbl, sort = "alphanumeric")
#' tbl
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
tbl_sort.tbl_hierarchical <- function(x, sort = "descending", ...) {
  set_cli_abort_call()

  ard_args <- attributes(x$cards$tbl_hierarchical)$args
  by_cols <- paste0("group", seq_along(length(ard_args$by)), c("", "_level"))

  # remove rows from ARD that do not correspond to a table row, append indices
  x_sort_match <- x$cards$tbl_hierarchical |>
    dplyr::filter(!is.na(group1)) |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::mutate(idx_sort = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    cards::as_card()
  attr(x_sort_match, "args") <- ard_args

  # pull indices each corresponding to one row of x$table_body
  pre_idx <- x_sort_match |>
    dplyr::pull("idx_sort") |>
    unique()

  # pull updated index order after sorting
  post_idx <- x_sort_match |>
    cards::ard_sort(sort) |>
    dplyr::pull("idx_sort") |>
    unique()

  # update x$cards
  x$cards$tbl_hierarchical <- x$cards$tbl_hierarchical |> cards::ard_sort(sort)

  # update x$table_body according to updated (relative) row positions
  x$table_body <- x$table_body[sapply(post_idx, function(x) which(pre_idx == x)), ]

  x
}
