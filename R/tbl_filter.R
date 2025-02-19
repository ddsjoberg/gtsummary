#' Filter Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter hierarchical table rows. Filters are not applied to summary or overall rows.
#'
#' @param x (`tbl_hierarchical`, `tbl_hierarchical_count`)\cr
#'   A hierarchical gtsummary table of class `'tbl_hierarchical'` or `'tbl_hierarchical_count'`.
#' @param keep_empty_summary (scalar `logical`)\cr
#'   Logical argument indicating whether to retain summary rows corresponding to table hierarchy sections that have had
#'   all rows filtered out. Default is `TRUE`.
#' @inheritParams cards::ard_filter
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
tbl_filter.tbl_hierarchical <- function(x, filter, keep_empty_summary = TRUE, ...) {
  set_cli_abort_call()

  ard_args <- attributes(x$cards$tbl_hierarchical)$args
  by_cols <- paste0("group", seq_along(length(ard_args$by)), c("", "_level"))
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
  x_ard_filter <- x_ard |> cards::ard_filter({{ filter }})

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

  # remove summary rows from empty sections if requested
  if (!keep_empty_summary) {
    if (length(ard_args$variables) > 1) {
      outer_cols <- ard_args$variables |> utils::head(-1)
      if (!dplyr::last(ard_args$variables) %in% x$table_body$variable) {
        x$table_body <- x$table_body |> dplyr::filter(!.data$variable %in% outer_cols)
        x_ard_filter <- x_ard_filter |> dplyr::filter(!.data$variable %in% outer_cols)
      } else {
        for (v in rev(outer_cols)) {
          empty_rows <- x$table_body |>
            dplyr::filter(.data$variable == dplyr::lead(.data$variable) & .data$variable == v) |>
            dplyr::pull("pre_idx")
          x$table_body <- x$table_body |> dplyr::filter(!.data$pre_idx %in% empty_rows)
          x_ard_filter <- x_ard_filter |> dplyr::filter(!.data$pre_idx %in% empty_rows)
        }
      }
    }
  }

  # update x$table_body
  x$table_body <- x$table_body |> select(-"pre_idx")

  # update x$cards
  x$cards$tbl_hierarchical <- x_ard_filter |> select(-"pre_idx")

  x
}
