#' Summary Table Bridges
#'
#' @description
#' Bridge function for converting `tbl_summary()` (and similar) cards to table bodies.
#' All bridge functions begin with prefix `brdg_*()`.
#'
#' This file also contains helper functions for constructing the bridge,
#' referred to as the piers (supports for a bridge) and begin with `pier_*()`.
#'
#' @param x gtsummary object of class `"tbl_summary"`
#' @param variables character list of variables
#' @param value named list of values to be summarized. the names are the
#' variable names.
#' @param calling_function string indicating the name of the function that is
#' calling thie bridge function, e.g. `call_function = "tbl_summary"`. This is used
#' internally to organize results.
#' @inheritParams tbl_summary
#'
#' @return data frame
#' @name bridge_summary
#'
#' @examples
#' tbl <-
#'   tbl_summary(
#'     data = mtcars,
#'     include = c("cyl", "am", "mpg", "hp"),
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
#' pier_summary_dichotomous(
#'  x = tbl,
#'  variables = "am",
#'  value = list(am = 1)
#' )
#'
#' pier_summary_categorical(
#'  x = tbl,
#'  variables = "cyl"
#' )
#'
#' pier_summary_continuous2(
#'  x = tbl,
#'  variables = "hp"
#' )
#'
#' pier_summary_continuous(
#'  x = tbl,
#'  variables = "mpg"
#' )
NULL

#' @rdname bridge_summary
#' @export
brdg_summary <- function(x, calling_function = "tbl_summary") {
  # add gts info to the cards table --------------------------------------------
  # add the function call column
  x$cards <-
    x$cards |>
    dplyr::mutate(
      gts_function =
        ifelse(
          .data$variable %in% c(x$inputs$tbl_summary$by, x$inputs$tbl_summary$include) &
            .data$context %in% c("continuous", "categorical"),
          .env$calling_function,
          NA_character_
        )
    )

  # adding the name of the column the stats will populate
  if (is_empty(x$inputs$by)) {
    x$cards$gts_column <-
      ifelse(x$cards$context %in% c("continuous", "categorical"), "stat_0", NA_character_)
  }
  else {
    x$cards <-
      x$cards |>
      dplyr::mutate(
        .by = cards::all_ard_groups(),
        gts_column =
          ifelse(
            .data$context %in% c("continuous", "categorical") &
              !.data$variable %in% .env$x$inputs$tbl_summary$by,
            paste0("stat_", dplyr::cur_group_id() - 1L),
            NA_character_
          )
      )
  }


  # build the table body pieces with bridge functions and stack them -----------
  x$table_body <-
    dplyr::left_join(
      dplyr::tibble(variable = x$inputs$include),
      dplyr::bind_rows(
        pier_summary_continuous(
          x,
          variables = .get_variables_by_type(x$inputs$type, type = "continuous")
        ),
        pier_summary_continuous2(
          x,
          variables = .get_variables_by_type(x$inputs$type, type = "continuous2")
        ),
        pier_summary_categorical(
          x,
          variables = .get_variables_by_type(x$inputs$type, type = "categorical")
        ),
        pier_summary_dichotomous(
          x,
          variables = .get_variables_by_type(x$inputs$type, type = "dichotomous")
        ),
        pier_summary_missing_row(x)
      ),
      by = "variable"
    )

  # construct default table_styling --------------------------------------------
  x <- construct_initial_table_styling(x)

  # add info to x$table_styling$header for dynamic headers ---------------------
  x <- .add_table_styling_stats(x)

  x
}

#' @rdname bridge_summary
#' @export
pier_summary_dichotomous <- function(x, variables, value = x$inputs$value) {
  if (is_empty(variables)) return(dplyr::tibble())
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

  pier_summary_continuous(x = x, variables = variables)
}

#' @rdname bridge_summary
#' @export
pier_summary_categorical <- function(x, variables, missing, missing_text, missing_stat) {
  if (is_empty(variables)) return(dplyr::tibble())
  # subsetting cards object on categorical summaries ----------------------------
  card <-
    x$cards |>
    dplyr::filter(.data$variable %in% .env$variables, .data$context %in% "categorical") |>
    cards::apply_statistic_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    card |>
    dplyr::group_by(across(c("gts_column", cards::all_ard_groups(), "variable"))) |>
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
          x$inputs$statistic[[df_groups_and_variable$variable[1]]]

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
      row_type = "level",
      var_label = unlist(.data$var_label),
      .after = 0L
    ) |>
    dplyr::select(-cards::all_ard_groups()) |>
    tidyr::unnest(cols = "df_stats") |>
    tidyr::unnest(cols = "stat") |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label"),
      names_from = "gts_column",
      values_from = "stat"
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

#' @rdname bridge_summary
#' @export
pier_summary_continuous2 <- function(x, variables, missing, missing_text, missing_stat) {
  if (is_empty(variables)) return(dplyr::tibble())
  # subsetting cards object on continuous2 summaries ----------------------------
  card <-
    x$cards |>
    dplyr::filter(.data$variable %in% .env$variables, .data$context %in% "continuous") |>
    cards::apply_statistic_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    card |>
    dplyr::group_by(across(c("gts_column", cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      function(.x, .y) {
        dplyr::mutate(
          .data = .y,
          stat =
            map(
              x$inputs$statistic[[.y$variable[1]]],
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
              x$inputs$statistic[[.y$variable[1]]],
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
      row_type = "level",
      var_label = unlist(.data$var_label),
      .after = 0L
    ) |>
    dplyr::select(-cards::all_ard_groups()) |>
    tidyr::unnest(cols = c("stat", "label")) |>
    tidyr::unnest(cols = c("stat", "label")) |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label"),
      names_from = "gts_column",
      values_from = "stat"
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

#' @rdname bridge_summary
#' @export
pier_summary_continuous <- function(x, variables, missing, missing_text, missing_stat) {
  # subsetting cards object on continuous summaries ----------------------------
  card <-
    x$cards |>
    dplyr::filter(.data$variable %in% .env$variables, .data$context %in% "continuous") |>
    cards::apply_statistic_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    card |>
    dplyr::group_by(across(c("gts_column", cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      ~dplyr::mutate(
        .data = .y,
        stat =
          glue::glue(
            x$inputs$statistic[[.data$variable[1]]],
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
      row_type = "header",
      var_label = unlist(.data$var_label),
      label = .data$var_label,
      .after = 0L
    ) |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label"),
      names_from = "gts_column",
      values_from = "stat"
    )

  df_results
}

#' @rdname bridge_summary
#' @export
pier_summary_missing_row <- function(x, variables = x$inputs$include) {
  if (is_empty(variables)) return(dplyr::tibble())
  # keeping variables to report missing obs for (or returning empty df if none)
  if (x$inputs$missing == "no") return(dplyr::tibble())
  if (x$inputs$missing == "ifany") {
    variables <-
      x$cards |>
      dplyr::filter(.data$stat_name == "N_miss", .data$variable %in% .env$variables) |>
      dplyr::filter(.data$statistic > 0L) |>
      dplyr::pull("variable") |>
      unique()
  }
  if (is_empty(variables)) return(dplyr::tibble())

  # slightly modifying the `x` object for missing value calculations
  x$inputs$statistic <- rep_named(variables, list(x$inputs$missing_stat))
  x$cards <-
    x$cards |>
    dplyr::mutate(
      context = ifelse(.data$context %in% "categorical", "continuous", .data$context)
    )


  pier_summary_continuous(x, variables = variables) |>
    dplyr::mutate(
      row_type = "missing",
      label = x$inputs$missing_text
    )
}

.add_table_styling_stats <- function(x) {
  if (is_empty(x$inputs$by)) {
    x$table_styling$header <-
      x$table_styling$header |>
      dplyr::mutate(
        modify_stat_N =
          x$cards |>
          dplyr::filter(.data$stat_name %in% "N_obs") |>
          dplyr::pull("statistic") |>
          unlist() |>
          getElement(1),
        modify_stat_n = .data$modify_stat_N,
        modify_stat_p = 1,
        modify_stat_level = "Overall"
      )
  }
  else {
    df_by_stats <- x$cards |>
      dplyr::filter(.data$variable %in% .env$x$inputs$by & .data$stat_name %in% c("N", "n", "p"))

    # get a data frame with the by variable stats
    df_by_stats_wide <-
      df_by_stats |>
      dplyr::filter(.data$stat_name %in% c("n", "p")) |>
      dplyr::mutate(
        .by = .data$variable_level,
        column = paste0("stat_", dplyr::cur_group_id())
      ) %>%
      {dplyr::bind_rows(
        .,
        dplyr::select(., "variable_level", "column", statistic = "variable_level") |>
          dplyr::mutate(stat_name = "level") |>
          dplyr::distinct()
      )} |>
      tidyr::pivot_wider(
        id_cols = "column",
        names_from = "stat_name",
        values_from = "statistic"
      ) |>
      dplyr::mutate(
        dplyr::across(-"column", unlist),
        dplyr::across("level", as.character)
      ) |>
      dplyr::rename_with(
        function(x) paste0("modify_stat_", x),
        .cols = -"column"
      )

    # add the stats here to the header data frame
    x$table_styling$header <-
      x$table_styling$header |>
      dplyr::mutate(
        modify_stat_N =
          df_by_stats |>
          dplyr::filter(.data$stat_name %in% "N") |>
          dplyr::pull("statistic") |>
          unlist()
      ) |>
      dplyr::left_join(
        df_by_stats_wide,
        by = "column"
      )
  }

  x
}
