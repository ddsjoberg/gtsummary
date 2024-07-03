#' Report statistics from gtsummary tables inline
#'
#' @param x (`gtsummary`)\cr
#'   Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @return A string reporting results from a gtsummary table
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @seealso [`inline_text.tbl_summary()`], [`inline_text.tbl_svysummary()`],
#' [`inline_text.tbl_regression()`], [`inline_text.tbl_uvregression()`],
#' [`inline_text.tbl_survfit()`], [`inline_text.tbl_cross()`], [`inline_text.gtsummary()`]
#'
#' @export
inline_text <- function(x, ...) {
  UseMethod("inline_text")
}

#' Report statistics from summary tables inline
#'
#' @param x (`gtsummary`)\cr
#'   gtsummary object
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single variable name of statistic to present
#' @param level (`string`)\cr
#'   Level of the variable to display for categorical variables.
#'   Default is `NULL`
#' @param column ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name to return from `x$table_body`.
#' @param pattern (`string`)\cr
#'   String indicating the statistics to return.
#'   Uses [`glue::glue()`] formatting. Default is `NULL`
#' @inheritParams rlang::args_dots_empty
#'
#' @export
#' @return A string
#' @name inline_text.gtsummary
#'
#' @section column + pattern:
#'
#' Some gtsummary tables report multiple statistics in a single cell,
#' e.g. `"{mean} ({sd})"` in `tbl_summary()` or `tbl_svysummary()`.
#' We often need to report just the mean or the SD, and that can be accomplished
#' by using both the `column=` and `pattern=` arguments. When both of these
#' arguments are specified, the column argument selects the column to report
#' statistics from, and the pattern argument specifies which statistics to report,
#' e.g. `inline_text(x, column = "stat_1", pattern = "{mean}")` reports just the
#' mean from a `tbl_summary()`. _This is not supported for all tables._
inline_text.gtsummary <- function(x,
                                  variable,
                                  level = NULL,
                                  column = NULL,
                                  pattern = NULL, ...) {
  set_cli_abort_call()
  check_dots_empty()
  check_not_missing(x)
  check_not_missing(variable)
  check_string(pattern, allow_empty = TRUE)

  cards::process_selectors(
    scope_table_body(x$table_body),
    variable = {{ variable }}
  )
  check_scalar(variable)

  cards::process_selectors(
    x$table_body,
    column = {{ column }}
  )
  # if `column` not passed, default to stat_0, if available
  if (is_empty(column)) {
    column <- intersect("stat_0", names(x$table_body))
  }
  check_scalar(column, allow_empty = TRUE)
  # must select column or pattern
  if (is_empty(column) && is_empty(pattern)) {
    cli::cli_abort("Arguments {.arg column} and {.arg pattern} cannot both be empty.")
  }

  # Deprecated in v2.0 use of the ci column
  if (!is_empty(pattern) && "ci" %in% .extract_glue_elements(pattern)) {
    cli::cli_warn(
      c("Use of the {.val ci} column was deprecated in {.pkg gtsummary} v2.0,
         and the column will eventually be removed from the tables.",
        i = "The {.val ci} column has been replaced by the merged {.val {c('conf.low', 'conf.high')}} columns (merged with {.fun modify_column_merge}).",
        i = "For patterns, replace {.val {{ci}}} with {.val {{conf.low}}, {{conf.high}}}.",
        i = "See {.help deprecated_ci_column} for details.")
    )
  }

  level <- enquo(level)

  # adding raw stats if user will use them -------------------------------------
  if (!is_empty(pattern) && !is_empty(column)) {
    # add overall results to ARD if called
    cards <- dplyr::bind_rows(x$cards[[1]], x$cards[["add_overall"]])

    # check structure of the cards object
    if (is_empty(cards) || !inherits(cards, "card") ||
        !"gts_column" %in% names(cards) || !column %in% cards$gts_column || !variable %in% cards$variable ||
        (!is_quo_empty(level) && !"variable_level" %in% names(cards))) {
      cli::cli_abort(
        "The structure of this table does not support use of the {.arg pattern} and {.arg column} argument both being specified.",
        call = get_cli_abort_call()
      )
    }

    # extract statistics from cards object
    cards <- cards |>
      cards::apply_fmt_fn() |>
      dplyr::filter(
        .data$variable %in% .env$variable,
        .data$gts_column %in% .env$column
      )

    # filter on variable level
    if (!is_quo_empty(level)) {
      level <-
        .select_levels(
          lvl = !!level,
          possible_lvls = map(unique(cards$variable_level), as.character) |> unlist(),
          lvl_argname =  "level",
          allow_empty = FALSE
        )
      cards <- cards |>
        dplyr::filter(map_lgl(.data$variable_level, ~ !is_empty(.x) && .x %in% level))
    }

    result <-
      cards::get_ard_statistics(cards, .column = "stat_fmt") |>
      glue::glue_data(.x = _, pattern)

    return(result)
  }

  # convert gtsummary object to tibble -----------------------------------------
  # removing merging and other styling
  x$table_styling$cols_merge <- dplyr::filter(x$table_styling$cols_merge, FALSE)
  x$table_styling$text_format <- dplyr::filter(x$table_styling$text_format, FALSE)
  # keeping all columns
  x$table_styling$header$hide <- FALSE

  df_gtsummary <- dplyr::as_tibble(x, col_labels = FALSE)

  # variable selection ---------------------------------------------------------
  df_gtsummary <- dplyr::filter(df_gtsummary, .data$variable %in% .env$variable)

  # check if tbl contains duplicate variable names
  if ("row_type" %in% names(df_gtsummary) &&
      nrow(df_gtsummary |> dplyr::filter(.data$row_type %in% "label")) > 1L) {
    cli::cli_inform(
      "Variable {.cal {variable}} likely not unique in gtsummary table, and
       the cell(s) you wish to display may not be accessible.
       This may occur when gtsummary tables with repeated variable
       names are combined using {.fun tbl_stack}.",
      call = get_cli_abort_call()
    )
  }

  # level selection ------------------------------------------------------------
  # if level not provided, keep the first row
  if (is_quo_empty(level)) {
    df_gtsummary <- dplyr::filter(df_gtsummary, dplyr::row_number() == 1)
  } # if there is a level, drop first label row, keeping the levels only
  else {
    if (any(!c("row_type", "label") %in% names(df_gtsummary))) {
      cli::cli_abort(
        "The gtsummary table does not have the required {.val {c('row_type', 'label')}} columns in {.code .$table_body}.",
        call = get_cli_abort_call()
      )
    }
    df_gtsummary <-
      dplyr::filter(df_gtsummary, !(.data$row_type %in% "label" & dplyr::row_number() == 1))
    level <-
      .select_levels(lvl = !!level, possible_lvls = df_gtsummary$label, lvl_argname =  "level", allow_empty = FALSE)
    df_gtsummary <- dplyr::filter(df_gtsummary, .data$label %in% .env$level)
  }

  # assert we've selected one line of table ------------------------------------
  if (nrow(df_gtsummary) != 1L) {
    cli::cli_abort("Criteria must select exactly one row.", call = get_cli_abort_call())
  }

  # cell selection -------------------------------------------------------------
  # if column selected and not pattern, return column
  if (!is_empty(column) && is_empty(pattern)) {
    return(df_gtsummary[[column]])
  }

  # if no column and pattern, return pattern
  if (!is.null(pattern)) {
    return(glue::glue_data(df_gtsummary, pattern) |> as.character())
  }
}

# after this deprecation cycle, we can replace this with arg_match()
.select_levels <- function(data, lvl, possible_lvls, lvl_argname =  caller_arg(lvl), allow_empty = TRUE) {
  lvl <- enquo(lvl)

  # if we allow empty and it's empty, return it
  if (allow_empty && is_quo_empty(lvl)) {
    lvl <- eval_tidy(lvl)
  }
  # if lvl is already character, use it as it is
  else if (tryCatch(is.character(eval_tidy(lvl)), error = \(x) FALSE)) {
    lvl <- eval_tidy(lvl)
  }
  else {
    lvl <- cards::cards_select(expr = !!lvl, data = vec_to_df(possible_lvls))
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = glue("gtsummary::inline_text({lvl_argname} = 'must now be a string')"),
      details = glue("Use `{lvl_argname} = '{lvl}'` instead.")
    )
  }

  if (allow_empty && is_empty(lvl)) return(lvl) # styler: off
  arg_match(lvl, values = possible_lvls, error_call = get_cli_abort_call(), error_arg = lvl_argname)
}


.update_fmt_fn <- function(cards, fmt_fn) {
  fmt_fn <- fmt_fn |> enframe("stat_name", "fmt_fn")
  cards <- cards |>
    map(
      function(x) {
        if (inherits(x, "card")) {
          return(
            dplyr::rows_update(
              x,
              fmt_fn,
              by = "stat_name",
              unmatched = "ignore"
            ) |>
              cards::apply_fmt_fn()
          )
        } else {
          return(x)
        }
      }
    )
}
