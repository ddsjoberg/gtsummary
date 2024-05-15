#' Add column with N
#'
#' - [`add_n.tbl_summary()`]
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param ... Passed to other methods.
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @export
#'
#' @seealso [`add_n.tbl_summary()`]
add_n <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_n")
}


#' Add column with N
#'
#' For each variable in a `tbl_summary` table, the `add_n` function adds a column with the
#' total number of non-missing (or missing) observations
#'
#' @param x (`tbl_summary`)\cr
#'   Object with class `'tbl_summary'` created with [`tbl_summary()`] function.
#' @param statistic (`string`)\cr
#'   String indicating the statistic to report. Default is the
#'   number of non-missing observation for each variable, `statistic = "{N_nonmiss}"`.
#'   All statistics available to report include:
#'
#'   * `"{N_obs}"` total number of observations,
#'   * `"{N_nonmiss}"` number of non-missing observations,
#'   * `"{N_miss}"` number of missing observations,
#'   * `"{p_nonmiss}"` percent non-missing data,
#'   * `"{p_miss}"` percent missing data
#'
#'   The argument uses [`glue::glue()`] syntax and multiple statistics may be reported,
#'   e.g. `statistic = "{N_nonmiss} / {N_obs} ({p_nonmiss}%)"`
#' @param col_label (`string`)\cr
#'   String indicating the column label.  Default is `"**N**"`
#' @param footnote (scalar `logical`)\cr
#'   Logical argument indicating whether to print a footnote
#'   clarifying the statistics presented. Default is `FALSE`
#' @param last (scalar `logical`)\cr
#'   Logical indicator to include N column last in table.
#'   Default is `FALSE`, which will display N column first.
#' @inheritParams rlang::args_dots_empty
#'
#' @author Daniel D. Sjoberg
#' @export
#'
#' @return A table of class `c('tbl_summary', 'gtsummary')`
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
#'   add_n()
add_n.tbl_summary <- function(x, statistic = "{N_nonmiss}", col_label = "**N**", footnote = FALSE,
                              last = FALSE, ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_string(statistic)
  check_string(col_label)
  check_scalar_logical(footnote)
  check_scalar_logical(last)

  # calculate the needed ARD results -------------------------------------------
  # TODO: Utilize themes to change the default formatting types
  # TODO: If `add_overall()` was previously run, we can get the stats from there instead of re-calculating
  x$cards$add_n <-
    cards::ard_missing(
      data = x$inputs$data,
      variables = x$inputs$include,
      by = character(0L),
      fmt_fn = ~list(starts_with("N_") ~ styfn_number(),
                     starts_with("p_") ~ styfn_percent())
    ) |>
    cards::apply_fmt_fn()

  # check statistic argument ---------------------------------------------------
  if (is_empty(.extract_glue_elements(statistic))) {
    cli::cli_abort(
      c("No glue elements found in the {.arg statistic} argument ({.val {statistic}}).",
        i = "Do you need to wrap the statistic name in curly brackets, e.g. {.val {{N_nonmiss}}}?"),
      call = get_cli_abort_call()
    )
  }
  if (any(!.extract_glue_elements(statistic) %in% x$cards$add_n$stat_name)) {
    missing_stats <- .extract_glue_elements(statistic) |> setdiff(x$cards$add_n$stat_name)
    cli::cli_abort(
      c("The following statistics are not valid for the {.arg statistic} argument: {.val {missing_stats}}.",
        i = "Select from {.val {unique(x$cards$add_n$stat_name)}}."),
      call = get_cli_abort_call()
    )
  }

  # prepare ARD data frame -----------------------------------------------------
  cards <-
    dplyr::bind_rows(
      x$cards$add_n,
      x[["cards"]][[1]] |>
        dplyr::filter(
          .data$variable %in% .env$x$inputs$include,
          .data$context %in% "attributes"
        )
    ) |>
    dplyr::mutate(
      gts_column = ifelse(.data$context %in% "missing", "n", NA_character_)
    ) %>%
    # adding `'{n}'` to ARD data frame
    # Prior to v2.0 release, the default value was `statistic="{n}"`
    # Documentation of {n} was removed in v2.0, so we can remove this chunk at some
    # point in the future. (May 2024)
    dplyr::bind_rows(
      dplyr::filter(
        .,
        .data$context %in% "missing",
        .data$stat_name %in% c("N_obs", "N_miss", "N_nonmiss", "p_nonmiss"),
      ) |>
        dplyr::mutate(
          stat_name =
            dplyr::case_when(
              .data$stat_name %in% "N_nonmiss" ~ "n",
              # documentation for the stats below were removed on 2022-04-03
              .data$stat_name %in% "N_obs" ~ "N",
              .data$stat_name %in% "N_miss" ~ "n_miss",
              .data$stat_name %in% "p_nonmiss" ~ "p"
            )
        )
    )

  # create tibble to merge with primary table ----------------------------------
  df_results <-
    pier_summary_dichotomous(
      cards = cards,
      variables = x$inputs$include,
      statistic = rep_named(x$inputs$include, x = list(statistic))
    ) |>
    dplyr::select("variable", "row_type", "n")

  # add results to primary table -----------------------------------------------
  x <- x |>
    modify_table_body(
      \(table_body) {
        table_body <-
          dplyr::left_join(
            table_body,
            df_results,
            by = c("variable", "row_type")
          )

        if (isFALSE(last)) {
          table_body <- dplyr::relocate(table_body, "n", .after = "label")
        }
      }
    ) |>
    modify_header(n = col_label)

  # add footnote if requested by user ------------------------------------------
  if (footnote) {
    footnote_text <-
      eval_tidy(
        expr = expr(glue(statistic)),
        data =
          x$cards$add_n |>
          dplyr::slice(.by = "stat_name", 1L) |>
          cards::get_ard_statistics(.column = "stat_label")
      )

    x <- modify_table_styling(x, columns = "n", footnote = footnote_text)
  }

  # return final table ---------------------------------------------------------
  x
}
