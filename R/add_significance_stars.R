#' Add significance stars
#'
#' Add significance stars to estimates with small p-values
#'
#' @param x (`gtsummary`)\cr
#'   A `'gtsummary'` object with a `'p.value'` column
#' @param thresholds (`numeric`)\cr
#'   Thresholds for significance stars. Default is `c(0.001, 0.01, 0.05)`
#' @param hide_ci (scalar `logical`)\cr
#'   logical whether to hide confidence interval. Default is `TRUE`
#' @param hide_p (scalar `logical`)\cr
#'   logical whether to hide p-value. Default is `TRUE` for regression summaries, and `FALSE` otherwise.
#' @param hide_se  (scalar `logical`)\cr
#'   logical whether to hide standard error. Default is `FALSE`
#' @param pattern (`string`)\cr
#'   glue-syntax string indicating what to display in formatted column.
#'   Default is `"{estimate}{stars}"` for regression summaries and `"{p.value}{stars}"` otherwise.
#'   A footnote is placed on the first column listed in the pattern.
#'   Other common patterns are
#'   `"{estimate}{stars} ({conf.low}, {conf.high})"` and
#'   `"{estimate} ({conf.low} to {conf.high}){stars}"`
#'
#' @export
#' @return a 'gtsummary' table
#'
#' @examplesIf gtsummary:::is_pkg_installed("car", reference_pkg = "gtsummary")
#' tbl <-
#'   lm(time ~ ph.ecog + sex, survival::lung) |>
#'   tbl_regression(label = list(ph.ecog = "ECOG Score", sex = "Sex"))
#'
#' # Example 1 ----------------------------------
#' tbl |>
#'   add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
#'
#' # Example 2 ----------------------------------
#' tbl |>
#'   add_significance_stars(
#'     pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
#'     hide_ci = TRUE, hide_se = TRUE
#'   ) |>
#'   modify_header(estimate = "**Beta (95% CI)**") |>
#'   modify_footnote(estimate = "CI = Confidence Interval", abbreviation = TRUE)
#'
#' # Example 3 ----------------------------------
#' # Use '  \n' to put a line break between beta and SE
#' tbl |>
#'   add_significance_stars(
#'     hide_se = TRUE,
#'     pattern = "{estimate}{stars}  \n({std.error})"
#'   ) |>
#'   modify_header(estimate = "**Beta  \n(SE)**") |>
#'   modify_footnote(estimate = "SE = Standard Error", abbreviation = TRUE) |>
#'   as_gt() |>
#'   gt::fmt_markdown(columns = everything()) |>
#'   gt::tab_style(
#'     style = "vertical-align:top",
#'     locations = gt::cells_body(columns = label)
#'   )
#'
#' # Example 4 ----------------------------------
#' lm(marker ~ stage + grade, data = trial) |>
#'   tbl_regression() |>
#'   add_global_p() |>
#'   add_significance_stars(
#'     hide_p = FALSE,
#'     pattern = "{p.value}{stars}"
#'   )
add_significance_stars <- function(x,
                                   pattern =
                                     ifelse(
                                       inherits(x, c("tbl_regression", "tbl_uvregression")),
                                       "{estimate}{stars}",
                                       "{p.value}{stars}"
                                     ),
                                   thresholds = c(0.001, 0.01, 0.05),
                                   hide_ci = TRUE,
                                   hide_p = inherits(x, c("tbl_regression", "tbl_uvregression")),
                                   hide_se = FALSE) {
  get_cli_abort_call()
  updated_call_list <- c(x$call_list, list(add_significance_stars = match.call()))

  # checking inputs ------------------------------------------------------------
  check_not_missing(x)
  check_class(x, "gtsummary")
  check_class(thresholds, "numeric")
  check_range(thresholds, range = c(0, 1), include_bounds = c(TRUE, TRUE))
  check_scalar_logical(hide_ci)
  check_scalar_logical(hide_p)
  check_scalar_logical(hide_se)
  if (!"p.value" %in% names(x$table_body)) {
    cli::cli_abort(
      "There is no p-value column in the table and significance stars cannot be placed.",
      call = get_cli_abort_call()
    )
  }

  # assign default pattern and footnote placement ------------------------------
  thresholds <- sort(thresholds, decreasing = TRUE) |> unique()

  pattern_cols <- .extract_glue_elements(pattern)
  if (is_empty(pattern_cols)) {
    cli::cli_abort(
      "The {.arg pattern} argumnet must be a string using glue syntax to select columns.",
      call = get_cli_abort_call()
    )
  }
  if (!"stars" %in% pattern_cols) {
    cli::cli_inform("The {.arg pattern} argument does not contain {.val {{stars}}} column, and no stars will be added.")
  }

  # adding footnote ------------------------------------------------------------
  p_footnote <-
    paste0(
      imap(thresholds, ~ rep_len("*", .y) %>% paste(collapse = "")),
      "p<",
      thresholds
    ) |>
    unlist() |>
    paste(collapse = "; ")

  x <- modify_footnote(x, any_of(pattern_cols[1]) ~ p_footnote)

  # adding stars column --------------------------------------------------------
  thresholds <- union(thresholds, 0L)
  expr_stars_case_when <-
    map2(
      thresholds, seq_along(thresholds),
      ~ expr(p.value >= !!.x ~ !!paste(rep_len("*", .y - 1), collapse = "")) |>
        expr_deparse()
    ) %>%
    reduce(.f = \(.x, .y) paste(.x, .y, sep = ", ")) %>%
    {paste0("dplyr::case_when(is.na(p.value) ~ '', ", ., ")")} |> # styler: off
    parse_expr()

  x <- modify_table_body(x, ~ .x |> dplyr::mutate(stars = !!expr_stars_case_when))

  # updating hidden column status ----------------------------------------------
  cols_to_hide <- c(conf.low = hide_ci, p.value = hide_p, std.error = hide_se)
  cols_to_hide <- cols_to_hide[c("conf.low", "p.value", "std.error") %in% names(x$table_body)]
  x <- x |>
    modify_table_styling(
      columns = all_of(names(cols_to_hide)),
      hide = cols_to_hide
    )

  # adding `cols_merge` to table styling ---------------------------------------
  x <- x |>
    modify_column_merge(
      rows = !is.na(.data$p.value),
      pattern = pattern
    )

  # return x -------------------------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x

}
