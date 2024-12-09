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
#' tbl_filter(tbl, t = 10)
#'
#' # Example 2 - Row Sums <= 5 ------------------
#' tbl_filter(tbl, t = 10, gt = FALSE, eq = TRUE)
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
tbl_filter.tbl_hierarchical <- function(x, t, gt = TRUE, eq = FALSE, .stat = "n", ...) {
  set_cli_abort_call()

  # process and check inputs ----------------------------------------------------------------------
  check_numeric(t)
  check_scalar_logical(gt)
  check_scalar_logical(eq)
  check_string(.stat)

  outer_cols <- sapply(
    x$table_body |> select(cards::all_ard_groups("names")),
    function(x) dplyr::last(unique(stats::na.omit(x)))
  )

  # get row sums ----------------------------------------------------------------------------------
  x <- .append_hierarchy_row_sums(x, .stat)

  # keep all summary rows (removed later if no sub-rows are kept)
  if (!gt) x$table_body$sum_row[x$table_body$variable %in% outer_cols] <- t - 1

  # create and apply filtering expression ---------------------------------------------------------
  filt_expr <- paste(
    "sum_row",
    dplyr::case_when(
      gt && eq ~ ">=",
      !gt && eq ~ "<=",
      !gt ~ "<",
      TRUE ~ ">"
    ),
    t
  )
  x$table_body <- x$table_body |>
    dplyr::filter(!!parse_expr(filt_expr))

  # remove any summary rows with no sub-rows still present ----------------------------------------
  if (!gt) {
    for (i in rev(seq_along(outer_cols))) {
      gp_empty <- x$table_body |>
        dplyr::group_by(across(c(names(outer_cols[1:i]), paste0(names(outer_cols[1:i]), "_level")))) |>
        dplyr::summarize(is_empty := dplyr::n() == 1) |>
        stats::na.omit()

      if (!all(!gp_empty$is_empty)) {
        x$table_body <- x$table_body |>
          dplyr::left_join(
            gp_empty,
            by = gp_empty |> select(cards::all_ard_groups()) |> names()
          ) |>
          dplyr::filter(!is_empty | is.na(is_empty)) |>
          dplyr::select(-"is_empty")
      } else {
        break
      }
    }
    if (nrow(x$table_body) > 0) {
      cli::cli_inform(
        "For readability, all summary rows preceding at least one row that meets the filtering criteria are kept
        regardless of whether they meet the filtering criteria themselves.",
        .frequency = "once",
        .frequency_id = "sum_rows_lt"
      )
    }
  }

  x$table_body <- x$table_body |>
    dplyr::select(-"sum_row")

  x
}
