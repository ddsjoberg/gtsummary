#' Filter Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter hierarchical table rows. Filters are not applied to summary or overall rows.
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   A hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @param filter (`expression`)\cr
#'   An expression that is used to filter rows of the table. See the Details section below.
#' @param keep_empty_summary (scalar `logical`)\cr
#'   Logical argument indicating whether to retain summary rows corresponding to table hierarchy sections that have had
#'   all rows filtered out. Default is `FALSE`.
#' @inheritParams rlang::args_dots_empty
#'
#' @details
#' The `filter` argument can be used to filter out rows of a table which do not meet the criteria provided as an
#' expression. Rows can be filtered on the values of any of the possible statistics (`n`, `p`, and `N`) provided they
#' are included at least once in the table, as well as the values of any `by` variables. Filtering is only applied to
#' rows that correspond to the innermost variable in the hierarchy - all outer variable (summary) rows are kept
#' regardless of whether they meet the filtering criteria themselves. In addition to filtering on individual statistic
#' values, filters can be applied across the row (i.e. across all `by` variable values) by using aggregate functions
#' such as `sum()` and `mean()`.
#'
#' If an overall column was added to the table (via `add_overall())`) this column will not be used in any filters (i.e.
#' `sum(n)` will not include the overall `n` in a given row). To filter on overall statistics use the `sum()` function
#' in your filter instead (i.e. `sum(n)` is equal to the overall column `n` across any `by` variables).
#'
#' Some examples of possible filters:
#' - `filter = n > 5`
#' - `filter = n == 2 & p < 0.05`
#' - `filter = sum(n) >= 4`
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
tbl_filter.tbl_hierarchical <- function(x, filter, keep_empty_summary = FALSE, ...) {
  set_cli_abort_call()

  ard_args <- attributes(x$cards$tbl_hierarchical)$args
  x_ard <- x$cards$tbl_hierarchical

  # add row indices to match structure of ard to x$table_body
  reshape_x <- .reshape_ard_compare(x, x_ard, ard_args)
  x <- reshape_x$x
  x_ard <- reshape_x$x_ard

  # get `by` variable count rows (do not correspond to a table row)
  rm_idx <- x_ard |>
    dplyr::filter(is.na(.data$group1)) |>
    dplyr::pull("pre_idx") |>
    unique()

  # apply filtering
  x_ard_filter <- x_ard |> cards::filter_ard_hierarchical({{ filter }}, keep_empty_summary)

  # pull updated index order after filtering
  idx_filter <- x_ard_filter |>
    dplyr::pull("pre_idx") |>
    unique() |>
    setdiff(rm_idx)

  # apply filtering while retaining original row order
  idx_filter <- intersect(x$table_body$pre_idx, idx_filter)
  x$table_body <- x$table_body[match(idx_filter, x$table_body$pre_idx), ]

  if ("tmp" %in% names(x_ard_filter)) {
    x_ard_filter <- x_ard_filter |>
      dplyr::filter(is.na(.data$tmp)) |>
      select(-"tmp")
  }

  # if overall column present, filter x$cards$add_overall
  if ("add_overall" %in% names(x$cards)) {
    x_ard_overall_col <- x$cards$add_overall |>
      dplyr::mutate(pre_idx = dplyr::row_number())

    # reformat data from overall column
    if (length(ard_args$by) > 0) {
      x_ard_overall_col <- x_ard_overall_col |>
        cards::rename_ard_groups_shift(shift = length(ard_args$by))
    }

    # check which rows are kept after filtering x$cards$tbl_hierarchical
    # and find matching rows in x$cards$add_overall
    by_cols <- paste0("group", seq_along(length(ard_args$by)), c("", "_level"))
    x_post_filter <- x_ard_filter |> select(cards::all_ard_groups(), cards::all_ard_variables(), -any_of(by_cols))
    idx_overall_filter <- x_ard_overall_col |>
      dplyr::inner_join(x_post_filter, by = names(x_post_filter), relationship = "many-to-many") |>
      dplyr::pull("pre_idx") |>
      unique()

    # keep total N row
    idx_overall_filter <- x_ard_overall_col$pre_idx |>
      intersect(c(idx_overall_filter, which(x_ard_overall_col$variable == "..ard_total_n..")))

    # update x$cards$add_overall
    x$cards$add_overall <- x$cards$add_overall[idx_overall_filter, ]
  }

  # update x$table_body
  x$table_body <- x$table_body |> select(-"pre_idx")

  # update x$cards$tbl_hierarchical
  x$cards$tbl_hierarchical <- x_ard_filter |> select(-"pre_idx")

  x
}
