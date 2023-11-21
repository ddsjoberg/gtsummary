#' Summary Table Bridges
#'
#' Bridge functions for converting `tbl_summary()` cards to table bodies.
#'
#' @param x gtsummary object of class `"tbl_summary"`
#' @param variables character list of variables
#' @param value named list of values to be summarized. the names are the
#' variable names.
#' @inheritParams tbl_summary
#'
#' @return data frame
#' @name bridge_tbl_summary
#'
#' @examples
#' tbl <-
#'   tbl_summary(
#'     data = mtcars,
#'     type =
#'       list(
#'         cyl = "categorical",
#'         am = "dichotomous",
#'         mpg = "continuous",
#'         hp = "continuous2"
#'       ),
#'     value = list(am = 1),
#'     statistic =
#'       list(
#'         c(cyl, am) ~ "{n} ({p}%)",
#'         mpg = "{mean} ({sd})",
#'         hp = c("{mean}", "{median}")
#'       )
#'   )
#'
#' bridge_summary_dichotomous(
#'  x = tbl,
#'  variables = "am",
#'  value = list(am = 1),
#'  missing = "ifany",
#'  missing_text = "Unknown",
#'  missing_stat = "{N_miss}"
#' )
#'
#' bridge_summary_categorical(
#'  x = tbl,
#'  variables = "cyl",
#'  missing = "ifany",
#'  missing_text = "Unknown",
#'  missing_stat = "{N_miss}"
#' )
#'
#' bridge_summary_continuous2(
#'  x = tbl,
#'  variables = "hp",
#'  missing = "ifany",
#'  missing_text = "Unknown",
#'  missing_stat = "{N_miss}"
#' )
#'
#' bridge_summary_continuous(
#'  x = tbl,
#'  variables = "mpg",
#'  missing = "ifany",
#'  missing_text = "Unknown",
#'  missing_stat = "{N_miss}"
#' )
NULL

#' @rdname bridge_tbl_summary
#' @export
bridge_summary_dichotomous <- function(x, variables, value, missing, missing_text, missing_stat) {
  # subsetting cards object on dichotomous summaries ---------------------------
  x$cards <-
    x$cards |>
    dplyr::filter(
      .by = c(cards::all_ard_groups(), "variable"),
      .data$variable %in% .env$variables,
      .data$variable_level %in% list(NULL) |
        .data$variable_level %in% list(value[[.data$variable[1]]])
    ) |>
    # updating the context to continuous, because it will be process that way below
    dplyr::mutate(
      context = ifelse(.data$context %in% "categorical", "continuous", .data$context)
    )

  bridge_summary_continuous(x = x, variables = variables)
}

#' @rdname bridge_tbl_summary
#' @export
bridge_summary_categorical <- function(x, variables, missing, missing_text, missing_stat) {
  # subsetting cards object on categorical summaries ----------------------------
  card <-
    x$cards |>
    dplyr::filter(.data$variable %in% .env$variables, .data$context %in% "categorical") |>
    cards::apply_statistic_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    card |>
    dplyr::group_by(across(c(cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      function(df_variable_stats, df_groups_and_variable) {
        lst_variable_stats <-
          cards::get_ard_statistics(
            df_variable_stats,
            .data$variable_level %in% list(NULL),
            .column = "statistic_fmt",
            .attributes = NULL
          )

        str_statistic_pre_glue <-
          x$calls$tbl_summary$statistic[[df_groups_and_variable$variable[1]]]

        dplyr::mutate(
          .data = df_groups_and_variable,
          df_stats =
            dplyr::filter(df_variable_stats, !.data$variable_level %in% list(NULL)) |>
            dplyr::group_by(.data$variable_level) |>
            dplyr::group_map(
              function(df_variable_level_stats, df_variable_levels) {
                dplyr::mutate(
                  .data = df_variable_levels,
                  stat =
                    map(
                      str_statistic_pre_glue,
                      function(str_to_glue) {
                        stat =
                          glue::glue(
                            str_to_glue,
                            .envir =
                              cards::get_ard_statistics(df_variable_level_stats, .column = "statistic_fmt", .attributes = NULL) |>
                              c(lst_variable_stats)
                          ) |>
                          as.character()

                      }
                    ),
                  label = unlist(.data$variable_level) |> as.character()
                )
              }
            ) |>
            dplyr::bind_rows() |>
            list()
        )
      }
    ) |>
    dplyr::bind_rows()

  # reshape results for final table --------------------------------------------
  df_result_levels <-
    df_glued |>
    # merge in variable label
    dplyr::left_join(
      x$cards |>
        dplyr::filter(.data$variable %in% .env$variables,
                      .data$context %in% "attributes",
                      .data$stat_name %in% "label") |>
        dplyr::select("variable", var_label = "statistic"),
      by = "variable"
    ) |>
    dplyr::mutate(
      .by = "variable",
      statistic_id = if (dplyr::n() == 1L) 0L else dplyr::row_number(),
      row_type = "level",
      var_label = unlist(.data$var_label),
      .after = 0L
    ) |>
    dplyr::select(-cards::all_ard_groups()) |>
    tidyr::unnest(cols = "df_stats") |>
    tidyr::unnest(cols = "stat") |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label"),
      names_from = "statistic_id",
      values_from = "stat",
      names_glue = "stat_{statistic_id}"
    )

  # add header rows to results -------------------------------------------------
  df_results <-
    map(
      variables,
      ~dplyr::bind_rows(
        df_result_levels |>
          dplyr::select("variable", "var_label", "row_type") |>
          dplyr::filter(.data$variable %in% .x) |>
          dplyr::filter(dplyr::row_number() %in% 1L) |>
          dplyr::mutate(
            label = .data$var_label,
            row_type = "header"
          ),
        df_result_levels |>
          dplyr::filter(.data$variable %in% .x)
      )
    ) |>
    dplyr::bind_rows()

  df_results
}

#' @rdname bridge_tbl_summary
#' @export
bridge_summary_continuous2 <- function(x, variables, missing, missing_text, missing_stat) {
  # subsetting cards object on continuous2 summaries ----------------------------
  card <-
    x$cards |>
    dplyr::filter(.data$variable %in% .env$variables, .data$context %in% "continuous") |>
    cards::apply_statistic_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    card |>
    dplyr::group_by(across(c(cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      function(.x, .y) {
        dplyr::mutate(
          .data = .y,
          stat =
            map(
              x$calls$tbl_summary$statistic[[.y$variable[1]]],
              function(str_to_glue) {
                stat =
                  glue::glue(
                    str_to_glue,
                    .envir = cards::get_ard_statistics(.x, .column = "statistic_fmt", .attributes = NULL)
                  ) |>
                  as.character()

              }
            ) |>
            list(),
          label =
            map(
              x$calls$tbl_summary$statistic[[.y$variable[1]]],
              function(str_to_glue) {
                label =
                  glue::glue(
                    str_to_glue,
                    .envir = cards::get_ard_statistics(.x, .column = "stat_label", .attributes = NULL)
                  ) |>
                  as.character()
              }
            ) |>
            list()
        )
      }
    ) |>
    dplyr::bind_rows()

  # reshape results for final table --------------------------------------------
  df_result_levels <-
    df_glued |>
    # merge in variable label
    dplyr::left_join(
      x$cards |>
        dplyr::filter(.data$variable %in% .env$variables,
                      .data$context %in% "attributes",
                      .data$stat_name %in% "label") |>
        dplyr::select("variable", var_label = "statistic"),
      by = "variable"
    ) |>
    dplyr::mutate(
      .by = "variable",
      statistic_id = if (dplyr::n() == 1L) 0L else dplyr::row_number(),
      row_type = "level",
      var_label = unlist(.data$var_label),
      .after = 0L
    ) |>
    dplyr::select(-cards::all_ard_groups()) |>
    tidyr::unnest(cols = c("stat", "label")) |>
    tidyr::unnest(cols = c("stat", "label")) |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label"),
      names_from = "statistic_id",
      values_from = "stat",
      names_glue = "stat_{statistic_id}"
    )

  # add header rows to results -------------------------------------------------
  df_results <-
    map(
      variables,
      ~dplyr::bind_rows(
        df_result_levels |>
          dplyr::select("variable", "var_label", "row_type") |>
          dplyr::filter(.data$variable %in% .x) |>
          dplyr::filter(dplyr::row_number() %in% 1L) |>
          dplyr::mutate(
            label = .data$var_label,
            row_type = "header"
          ),
        df_result_levels |>
          dplyr::filter(.data$variable %in% .x)
      )
    ) |>
    dplyr::bind_rows()

  df_results
}

#' @rdname bridge_tbl_summary
#' @export
bridge_summary_continuous <- function(x, variables, missing, missing_text, missing_stat) {
  # subsetting cards object on continuous summaries ----------------------------
  card <-
    x$cards |>
    dplyr::filter(.data$variable %in% .env$variables, .data$context %in% "continuous") |>
    cards::apply_statistic_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    card |>
    dplyr::group_by(across(c(cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      ~dplyr::mutate(
        .data = .y,
        stat =
          glue::glue(
            x$calls$tbl_summary$statistic[[.data$variable[1]]],
            .envir = cards::get_ard_statistics(.x, .column = "statistic_fmt", .attributes = NULL)
          ) |>
          as.character()
      )
    ) |>
    dplyr::bind_rows()

  # reshape results for final table --------------------------------------------
  df_results <-
    df_glued |>
    # merge in variable label
    dplyr::left_join(
      x$cards |>
        dplyr::filter(.data$variable %in% .env$variables,
                      .data$context %in% "attributes",
                      .data$stat_name %in% "label") |>
        dplyr::select("variable", var_label = "statistic"),
      by = "variable"
    ) |>
    dplyr::mutate(
      .by = "variable",
      statistic_id = if (dplyr::n() == 1L) 0L else dplyr::row_number(),
      row_type = "header",
      var_label = unlist(.data$var_label),
      label = .data$var_label,
      .after = 0L
    ) |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label"),
      names_from = "statistic_id",
      values_from = "stat",
      names_glue = "stat_{statistic_id}"
    )

  df_results
}
