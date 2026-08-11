#' Add rate differences to hierarchical tables
#'
#' @description
#' Add a column of event-rate differences to a table created with
#' [`tbl_hierarchical()`] or [`tbl_ard_hierarchical()`]. For every node in the
#' hierarchy (e.g. each system organ class and each preferred term) the event
#' rate of a second `by` group is subtracted from the rate of a first `by` group.
#' This is a wrapper around `cards::diff_ard_hierarchical()`.
#'
#' The table must be stratified by a single `by` variable and its statistics must
#' include the rate (`p`) statistic (the default for `tbl_hierarchical()`).
#'
#' @param x (`tbl_hierarchical`/`tbl_ard_hierarchical`)\cr
#'   table created with [`tbl_hierarchical()`] or [`tbl_ard_hierarchical()`].
#' @param levels (`vector`)\cr
#'   a length-two vector of the `by` variable levels to compare. The difference
#'   is calculated as `levels[1]` minus `levels[2]`. This argument is required
#'   when the `by` variable has more than two levels, and when `by` has exactly
#'   two levels it is optional and can be used to flip the direction of the
#'   difference. Default is `NULL`.
#' @param statistic (`string`)\cr
#'   a single glue string defining the difference statistic to display. The only
#'   available element is `{estimate}` (the rate difference). Default is
#'   `"{estimate}%"`.
#' @param estimate_fun (`function`)\cr
#'   a function to round and format the rate difference. For
#'   `add_difference.tbl_hierarchical()` the default is
#'   `label_style_number(digits = 1, scale = 100)`. For
#'   `add_difference.tbl_ard_hierarchical()` the default is `NULL`, meaning the
#'   formatting function carried in the source ARD is used.
#' @inheritParams rlang::args_dots_empty
#'
#' @return a gtsummary table of the same class as `x`
#' @name add_difference.tbl_hierarchical
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' # Example 1 ----------------------------------
#' # rate difference between two treatment arms
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(AESOC %in% unique(cards::ADAE$AESOC)[1:5])
#'
#' tbl_hierarchical(
#'   data = ADAE_subset,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = cards::ADSL,
#'   id = USUBJID
#' ) |>
#'   add_difference(levels = c("Xanomeline High Dose", "Placebo"))
NULL

#' @rdname add_difference.tbl_hierarchical
#' @export
add_difference.tbl_hierarchical <- function(x,
                                            levels = NULL,
                                            statistic = "{estimate}%",
                                            estimate_fun = label_style_number(digits = 1, scale = 100),
                                            ...) {
  set_cli_abort_call()
  check_dots_empty()
  .add_difference_hierarchical(
    x = x, levels = levels, statistic = statistic,
    estimate_fun = estimate_fun, call = match.call()
  )
}

#' @rdname add_difference.tbl_hierarchical
#' @export
add_difference.tbl_ard_hierarchical <- function(x,
                                                levels = NULL,
                                                statistic = "{estimate}%",
                                                estimate_fun = NULL,
                                                ...) {
  set_cli_abort_call()
  check_dots_empty()
  .add_difference_hierarchical(
    x = x, levels = levels, statistic = statistic,
    estimate_fun = estimate_fun, call = match.call()
  )
}

# shared worker for the hierarchical `add_difference()` methods.
# `estimate_fun = NULL` means "use the formatting function already stored in the
# source ARD" (the `tbl_ard_hierarchical()` default); a non-`NULL` value overrides it.
.add_difference_hierarchical <- function(x, levels, statistic, estimate_fun, call) {
  # check/process inputs -------------------------------------------------------
  check_string(statistic)

  # cannot run twice
  if ("add_difference" %in% names(x$call_list)) {
    cli::cli_abort(
      "The {.fun add_difference} function has already been called and cannot be called again.",
      call = get_cli_abort_call()
    )
  }
  # capture the call list now so the intermediate `modify_*()` calls below do not
  # pollute it; restored (with `add_difference` appended) at the end
  updated_call_list <- c(x$call_list, list(add_difference = call))

  cls <- class(x)[1]
  x_ard <- x$cards[[cls]]
  ard_args <- attributes(x_ard)$args
  by_var <- ard_args$by %||% x$inputs$by

  # must be stratified by a single `by` variable
  if (is_empty(by_var)) {
    cli::cli_abort(
      c("Cannot run {.fun add_difference} when the table is not stratified by a
         {.arg by} variable.",
        i = "Rebuild the table with {.code {cls}(by)}."
      ),
      call = get_cli_abort_call()
    )
  }

  # translate the length-two `levels` vector into the flat named-list form that
  # `cards::diff_ard_hierarchical()` expects. Level validity is delegated to cards.
  levels_list <- NULL
  if (!is_empty(levels)) {
    if (length(levels) != 2L) {
      cli::cli_abort(
        c("The {.arg levels} argument must be a length-two vector.",
          i = "It has length {length(levels)}."
        ),
        call = get_cli_abort_call()
      )
    }
    if (anyNA(levels) || identical(as.character(levels[1]), as.character(levels[2]))) {
      cli::cli_abort(
        "The {.arg levels} argument must contain two distinct, non-missing values.",
        call = get_cli_abort_call()
      )
    }
    levels_list <-
      stats::setNames(as.list(as.character(levels)), rep(by_var, length(levels)))
  }

  # calculate the rate differences ---------------------------------------------
  ard_diff <- cards::diff_ard_hierarchical(x_ard, levels = levels_list)

  # reshape the difference ARD into a single formatted column -------------------
  ard_diff$gts_column <- "estimate"
  # clear the stale `stat_fmt` (carried over from the `p` statistic) so the
  # formatting function is re-applied downstream in `pier_summary_hierarchical()`
  ard_diff$stat_fmt <- list(NULL)
  # when supplied, override the ARD's formatting function; otherwise keep the
  # function stored in the source ARD (the `tbl_ard_hierarchical()` default)
  if (!is_empty(estimate_fun)) {
    ard_diff$fmt_fun <- list(as_function(estimate_fun))
  }
  ard_diff <- cards::as_card(ard_diff, check = FALSE)

  statistic_list <-
    rep(list(statistic), length(ard_args$variables)) |>
    stats::setNames(ard_args$variables)

  piece <-
    pier_summary_hierarchical(
      cards = ard_diff,
      variables = ard_args$variables,
      include = ard_args$include,
      statistic = statistic_list
    )

  # merge the new column into the table body, aligning on hierarchy identity ----
  # (the join is by hierarchy keys, not row position, so it is robust to a prior
  # `sort_hierarchical()`/`filter_hierarchical()` call)
  group_cols <- piece |> dplyr::select(cards::all_ard_groups()) |> names()
  join_keys <- intersect(c("variable", "label", group_cols), names(x$table_body))
  x <-
    modify_table_body(
      x,
      dplyr::left_join,
      piece[c(join_keys, "estimate")],
      by = join_keys
    )

  # header, footnote, and unhide the new column --------------------------------
  diff_spec <- attributes(ard_diff)$args$diff
  lvl1 <- as.character(diff_spec$group1[[by_var]])
  lvl2 <- as.character(diff_spec$group2[[by_var]])
  x <-
    modify_table_styling(
      x,
      columns = "estimate",
      label = glue("**{translate_string('Rate Difference')}**"),
      hide = FALSE,
      footnote = glue("{translate_string('Difference')}: {lvl1} - {lvl2}")
    )

  # save the difference ARD and update the call list ---------------------------
  x$cards[["add_difference"]] <- ard_diff
  x$call_list <- updated_call_list

  # print any conditions captured while calculating the differences
  cards::print_ard_conditions(ard_diff)

  x
}
