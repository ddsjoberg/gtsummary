#' Filter Hierarchical Tables
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter hierarchical tables by total row frequencies.
#'
#' @param x (`tbl_hierarchical`)\cr
#'   A hierarchical gtsummary table of class `'tbl_hierarchical'`.
#' @param t (scalar `numeric`)\cr
#'   Threshold used to determine which values will be retained.
#' @param gt (scalar `logical`)\cr
#'   Whether to filter for row sums greater than `t` or less than `t`. Default is greater than (`gt = TRUE`).
#' @param eq (scalar `logical`)\cr
#'   Whether to include the value of `t` in the filtered range, i.e. whether to use exclusive comparators (`>`, `<`) or
#'   inclusive comparators (`>=`, `<=`) when filtering. Default is `FALSE`.
#' @param .stat (`string`)\cr
#'   Statistic to use to calculate row sums. This statistic must be present in the table for all hierarchy levels.
#' @name filter_tbl_hierarchical
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
tbl_filter.tbl_hierarchical <- function(x, t, gt = TRUE, eq = FALSE, .stat = "n") {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_numeric(t)
  check_scalar_logical(gt)
  check_scalar_logical(eq)
  check_string(.stat)

  x <- .append_hierarchy_row_sums(x, .stat)
  g_cols <- sapply(x$table_body |> select(cards::all_ard_groups("names")), function(x) tail(unique(na.omit(x)), 1))
  if (!gt) x$table_body$count_total[x$table_body$variable %in% g_cols] <- t - 1

  filt_expr <- paste(
    "count_total",
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

  # remove summary rows with no sub-rows still present -------------------------
  if (!gt) {
    for (i in rev(seq_along(g_cols))) {
      empty_gps <- x$table_body |>
        dplyr::group_by(across(c(names(g_cols[1:i]), paste0(names(g_cols[1:i]), "_level")))) |>
        dplyr::summarize(empty_gp := dplyr::n() == 1) |>
        na.omit()

      if (!all(!empty_gps$empty_gp)) {
        cli::cli_inform(
          "For readability, any summary row that supercedes a row that meets the filtering criteria will be kept
          regardless of whether it meets the filtering criteria itself.",
          .frequency = "once",
          .frequency_id = "hierarchy_filter_lt"
        )

        x$table_body <- x$table_body |>
          dplyr::left_join(
            empty_gps,
            by = empty_gps |> select(cards::all_ard_groups()) |> names()
          ) |>
          dplyr::filter(!empty_gp | is.na(empty_gp)) |>
          dplyr::select(-"empty_gp")
      } else {
        break
      }
    }
  }

  x$table_body <- x$table_body |>
    dplyr::select(-"count_total")

  x
}
