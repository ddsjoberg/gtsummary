#' Summary table bridge
#'
#' @description
#' Bridge function for converting `tbl_summary()` (and similar) cards to basic gtsummary objects.
#' All bridge functions begin with prefix `brdg_*()`.
#'
#' This file also contains helper functions for constructing the bridge,
#' referred to as the piers (supports for a bridge) and begin with `pier_*()`.
#'
#' - `brdg_summary()`: The bridge function ingests an ARD data frame and returns
#'   a gtsummary table that includes `.$table_body` and a basic `.$table_styling`.
#'   The `.$table_styling$header` data frame includes the header statistics.
#'   Based on context, this function adds a column to the ARD data frame named
#'   `"gts_column"`. This column is used during the reshaping in the `pier_*()`
#'   functions defining column names.
#'
#' - `pier_*()`: these functions accept a cards tibble and returns a tibble
#'   that is a piece of the `.$table_body`. Typically these will be stacked
#'   to construct the final table body data frame. The ARD object passed here
#'   will have two primary parts: the calculated summary statistics and the
#'   attributes ARD. The attributes ARD is used for labeling. The ARD data frame
#'   passed to this function must include a `"gts_column"` column, which is
#'   added in `brdg_summary()`.
#'
#' @param cards (`card`)\cr
#'   An ARD object of class `"card"` typically created with `cards::ard_*()` functions.
#' @param variables (`character`)\cr
#'   character list of variables
#' @param by (`string`)\cr
#'   string indicating the stratifying column
#' @param type (named `list`)\cr
#'   named list of summary types
#' @param statistic (named `list`)\cr
#'   named list of summary statistic names
#' @inheritParams tbl_summary
#'
#' @return a gtsummary object
#' @name brdg_summary
#'
#' @examples
#' library(cards)
#'
#' # first build ARD data frame
#' cards <-
#'   ard_stack(
#'     mtcars,
#'     ard_continuous(variables = c("mpg", "hp")),
#'     ard_categorical(variables = "cyl"),
#'     ard_dichotomous(variables = "am"),
#'     .missing = TRUE,
#'     .attributes = TRUE
#'   ) |>
#'   # this column is used by the `pier_*()` functions
#'   dplyr::mutate(gts_column = ifelse(context == "attributes", NA, "stat_0"))
#'
#' brdg_summary(
#'   cards = cards,
#'   variables = c("cyl", "am", "mpg", "hp"),
#'   type =
#'     list(
#'       cyl = "categorical",
#'       am = "dichotomous",
#'       mpg = "continuous",
#'       hp = "continuous2"
#'     ),
#'   statistic =
#'     list(
#'       cyl = "{n} / {N}",
#'       am = "{n} / {N}",
#'       mpg = "{mean} ({sd})",
#'       hp = c("{median} ({p25}, {p75})", "{mean} ({sd})")
#'     )
#' ) |>
#'   as_tibble()
#'
#' pier_summary_dichotomous(
#'   cards = cards,
#'   variables = "am",
#'   statistic = list(am = "{n} ({p})")
#' )
#'
#' pier_summary_categorical(
#'   cards = cards,
#'   variables = "cyl",
#'   statistic = list(cyl = "{n} ({p})")
#' )
#'
#' pier_summary_continuous2(
#'   cards = cards,
#'   variables = "hp",
#'   statistic = list(hp = c("{median}", "{mean}"))
#' )
#'
#' pier_summary_continuous(
#'   cards = cards,
#'   variables = "mpg",
#'   statistic = list(mpg = "{median}")
#' )
NULL

#' @rdname brdg_summary
#' @export
brdg_summary <- function(cards,
                         variables,
                         type,
                         statistic,
                         by = NULL,
                         missing = "no",
                         missing_stat = "{N_miss}",
                         missing_text = "Unknown") {
  set_cli_abort_call()

  # build the table body pieces with bridge functions and stack them -----------
  table_body <-
    dplyr::left_join(
      dplyr::tibble(
        variable = variables,
        var_type = type[.data$variable] |> unlist() |> unname()
      ),
      dplyr::bind_rows(
        pier_summary_continuous(
          cards = cards,
          variables = .get_variables_by_type(type, type = "continuous"),
          statistic = statistic
        ),
        pier_summary_continuous2(
          cards = cards,
          variables = .get_variables_by_type(type, type = "continuous2"),
          statistic = statistic
        ),
        pier_summary_categorical(
          cards = cards,
          variables = .get_variables_by_type(type, type = "categorical"),
          statistic = statistic
        ),
        pier_summary_dichotomous(
          cards = cards,
          variables = .get_variables_by_type(type, type = "dichotomous"),
          statistic = statistic
        ),
        pier_summary_missing_row(
          cards = cards,
          variables = variables,
          missing = missing,
          missing_stat = missing_stat,
          missing_text = missing_text
        )
      ),
      by = "variable"
    )

  # construct default table_styling --------------------------------------------
  x <- .create_gtsummary_object(table_body)

  # add info to x$table_styling$header for dynamic headers ---------------------
  x <- .add_table_styling_stats(x, cards = cards, by = by)

  # adding styling -------------------------------------------------------------
  x <- x |>
    # add header to label column and add default indentation
    modify_table_styling(
      columns = "label",
      label = glue("**{translate_string('Characteristic')}**"),
      rows = .data$row_type %in% c("level", "missing"),
      indent = 4L
    ) |>
    # adding the statistic footnote
    modify_table_styling(
      columns = all_stat_cols(),
      footnote =
        .construct_summary_footnote(cards, variables, statistic, type)
    )

  x |>
    structure(class = "gtsummary") |>
    modify_column_unhide(columns = all_stat_cols())
}

#' @rdname brdg_summary
#' @export
pier_summary_dichotomous <- function(cards,
                                     variables,
                                     statistic) {
  set_cli_abort_call()
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }

  pier_summary_continuous(
    cards = cards,
    variables = variables,
    statistic = statistic
  )
}

#' @rdname brdg_summary
#' @export
pier_summary_categorical <- function(cards,
                                     variables,
                                     statistic) {
  set_cli_abort_call()
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }
  # subsetting cards object on categorical summaries ----------------------------
  cards_no_attr <-
    cards |>
    dplyr::filter(.data$variable %in% .env$variables, !.data$context %in% "attributes") |>
    cards::apply_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    cards_no_attr |>
    dplyr::group_by(across(c("gts_column", cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      function(df_variable_stats, df_groups_and_variable) {
        lst_variable_stats <-
          cards::get_ard_statistics(
            df_variable_stats,
            .data$variable_level %in% list(NULL),
            .column = "stat_fmt"
          )

        str_statistic_pre_glue <-
          statistic[[df_groups_and_variable$variable[1]]]

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
                        stat <-
                          glue::glue_data(
                            .x =
                              cards::get_ard_statistics(df_variable_level_stats, .column = "stat_fmt") |>
                              c(lst_variable_stats),
                            str_to_glue
                          ) |>
                          as.character()
                      }
                    ),
                  label = map_chr(.data$variable_level, as.character)
                )
              }
            ) |>
            dplyr::bind_rows() |>
            list()
        )
      }
    ) |>
    dplyr::bind_rows() %>%
    # this ensures the correct order when there are 10+ groups
    dplyr::left_join(
      cards_no_attr |> dplyr::distinct(!!sym("gts_column")),
      .,
      by = "gts_column"
    )

  # reshape results for final table --------------------------------------------
  df_result_levels <-
    df_glued |>
    # merge in variable label
    dplyr::left_join(
      cards |>
        dplyr::filter(
          .data$variable %in% .env$variables,
          .data$context %in% "attributes",
          .data$stat_name %in% "label"
        ) |>
        dplyr::select("variable", var_label = "stat"),
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
      ~ dplyr::bind_rows(
        df_result_levels |>
          dplyr::select("variable", "var_label", "row_type") |>
          dplyr::filter(.data$variable %in% .x) |>
          dplyr::filter(dplyr::row_number() %in% 1L) |>
          dplyr::mutate(
            label = .data$var_label,
            row_type = "label"
          ),
        df_result_levels |>
          dplyr::filter(.data$variable %in% .x)
      )
    ) |>
    dplyr::bind_rows()

  df_results
}

#' @rdname brdg_summary
#' @export
pier_summary_continuous2 <- function(cards,
                                     variables,
                                     statistic) {
  set_cli_abort_call()
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }
  # subsetting cards object on continuous2 summaries ----------------------------
  cards_no_attr <-
    cards |>
    dplyr::filter(.data$variable %in% .env$variables, !.data$context %in% "attributes") |>
    cards::apply_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    cards_no_attr |>
    dplyr::group_by(across(c("gts_column", cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      function(.x, .y) {
        dplyr::mutate(
          .data = .y,
          stat =
            map(
              statistic[[.y$variable[1]]],
              function(str_to_glue) {
                stat <-
                  glue::glue_data(
                    .x = cards::get_ard_statistics(.x, .column = "stat_fmt"),
                    str_to_glue
                  ) |>
                  as.character()
              }
            ) |>
            list(),
          label =
            map(
              statistic[[.y$variable[1]]],
              function(str_to_glue) {
                label <-
                  glue::glue_data(
                    .x = cards::get_ard_statistics(.x, .column = "stat_label"),
                    str_to_glue
                  ) |>
                  as.character()
              }
            ) |>
            list()
        )
      }
    ) |>
    dplyr::bind_rows() %>%
    # this ensures the correct order when there are 10+ groups
    dplyr::left_join(
      cards_no_attr |> dplyr::distinct(!!sym("gts_column")),
      .,
      by = "gts_column"
    )

  # reshape results for final table --------------------------------------------
  df_result_levels <-
    df_glued |>
    # merge in variable label
    dplyr::left_join(
      cards |>
        dplyr::filter(
          .data$variable %in% .env$variables,
          .data$context %in% "attributes",
          .data$stat_name %in% "label"
        ) |>
        dplyr::select("variable", var_label = "stat"),
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
      ~ dplyr::bind_rows(
        df_result_levels |>
          dplyr::select("variable", "var_label", "row_type") |>
          dplyr::filter(.data$variable %in% .x) |>
          dplyr::filter(dplyr::row_number() %in% 1L) |>
          dplyr::mutate(
            label = .data$var_label,
            row_type = "label"
          ),
        df_result_levels |>
          dplyr::filter(.data$variable %in% .x)
      )
    ) |>
    dplyr::bind_rows()

  df_results
}

#' @rdname brdg_summary
#' @export
pier_summary_continuous <- function(cards,
                                    variables,
                                    statistic) {
  set_cli_abort_call()
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }
  # subsetting cards object on statistical summaries ---------------------------
  cards_no_attr <-
    cards |>
    dplyr::filter(.data$variable %in% .env$variables, !.data$context %in% "attributes") |>
    cards::apply_fmt_fn()

  # construct formatted statistics ---------------------------------------------
  df_glued <-
    # construct stat columns with glue by grouping variables and primary summary variable
    cards_no_attr |>
    dplyr::group_by(across(c("gts_column", cards::all_ard_groups(), "variable"))) |>
    dplyr::group_map(
      function(.x, .y) {
        dplyr::mutate(
          .data = .y,
          stat =
            glue::glue_data(
              .x = cards::get_ard_statistics(.x, .column = "stat_fmt"),
              statistic[[.data$variable[1]]]
            ) |>
            as.character()
        )
      }
    ) |>
    dplyr::bind_rows() %>%
    # this ensures the correct order when there are 10+ groups
    dplyr::left_join(
      cards_no_attr |> dplyr::distinct(!!sym("gts_column")),
      .,
      by = "gts_column"
    )

  # reshape results for final table --------------------------------------------
  df_results <-
    df_glued |>
    # merge in variable label
    dplyr::left_join(
      cards |>
        dplyr::filter(
          .data$variable %in% .env$variables,
          .data$context %in% "attributes",
          .data$stat_name %in% "label"
        ) |>
        dplyr::select("variable", var_label = "stat"),
      by = "variable"
    ) |>
    dplyr::mutate(
      .by = "variable",
      row_type = "label",
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

#' @rdname brdg_summary
#' @export
pier_summary_missing_row <- function(cards,
                                     variables,
                                     missing = "no",
                                     missing_stat = "{N_miss}",
                                     missing_text = "Unknown") {
  set_cli_abort_call()

  # return empty tibble if no missing row requested
  if (is_empty(variables) || missing == "no") {
    return(dplyr::tibble())
  }

  # if "ifany", replace the variables vector with those that have missing values
  if (missing == "ifany") {
    variables <-
      cards |>
      dplyr::filter(.data$stat_name == "N_miss", .data$variable %in% .env$variables) |>
      dplyr::filter(.data$stat > 0L) |>
      dplyr::pull("variable") |>
      unique()
  }

  # slightly modifying the `x` object for missing value calculations -----------
  # make all the summary stats the same for all vars
  statistic <- rep_named(variables, list(missing_stat))

  # reshape the missing stats
  pier_summary_continuous(
    cards = cards,
    variables = variables,
    statistic = statistic
  ) |>
    # update the row_type and label
    dplyr::mutate(
      row_type = "missing",
      label = missing_text
    )
}

.add_table_styling_stats <- function(x, cards, by) {
  if (is_empty(by)) {
    x$table_styling$header$modify_stat_level <- translate_string("Overall")

    # add overall N to x$table_styling$header
    lst_total_n <- cards::get_ard_statistics(cards, .data$variable %in% "..ard_total_n..")
    if ("N" %in% names(lst_total_n)) {
      x$table_styling$header <-
        x$table_styling$header |>
        dplyr::mutate(
          modify_stat_N = lst_total_n[["N"]],
          modify_stat_n = .data$modify_stat_N,
          modify_stat_p = 1
        )
    }

    # if this is a survey object, then add unweighted stats as well
    if ("N_unweighted" %in% names(lst_total_n)) {
      x$table_styling$header <-
        x$table_styling$header |>
        dplyr::mutate(
          modify_stat_N_unweighted = lst_total_n[["N_unweighted"]],
          modify_stat_n_unweighted = .data$modify_stat_N_unweighted,
          modify_stat_p_unweighted = 1
        )
    }
  }
  # add by variable stats
  else {
    df_by_stats <- cards |>
      dplyr::filter(
        .data$variable %in% .env$by,
        .data$stat_name %in% c("N", "n", "p", "N_unweighted", "n_unweighted", "p_unweighted")
      )

    # if no tabulation of the 'by' variable provided, just return the 'by' levels
    if (nrow(df_by_stats) == 0L) {
      df_by_stats_wide <-
        cards |>
        dplyr::select(column = "gts_column", modify_stat_level = "group1_level") |>
        dplyr::distinct() |>
        dplyr::filter(!is.na(.data$column) & !map_lgl(.data$modify_stat_level, is.null)) |>
        dplyr::mutate(across(everything(), ~unlist(.) |> as.character()))
    }
    # otherwise prepare the tabulation stats
    else {
      df_by_stats_wide <-
        df_by_stats |>
        dplyr::filter(.data$stat_name %in% c("N", "n", "p", "N_unweighted", "n_unweighted", "p_unweighted")) |>
        dplyr::select(cards::all_ard_variables(), "stat_name", "stat") |>
        dplyr::left_join(
          cards |>
            dplyr::select(cards::all_ard_groups(), "gts_column") |>
            dplyr::filter(!is.na(.data$gts_column)) |>
            dplyr::distinct() |>
            dplyr::rename(variable = "group1", variable_level = "group1_level"),
          by = c("variable", "variable_level")
        ) %>%
        dplyr::bind_rows(
          dplyr::select(., "variable_level", "gts_column", stat = "variable_level") |>
            dplyr::mutate(stat_name = "level") |>
            dplyr::distinct()
        ) |>
        tidyr::pivot_wider(
          id_cols = "gts_column",
          names_from = "stat_name",
          values_from = "stat"
        ) |>
        dplyr::mutate(
          dplyr::across(-"gts_column", unlist),
          dplyr::across("level", as.character)
        ) |>
        dplyr::rename_with(
          function(x) paste0("modify_stat_", x),
          .cols = -"gts_column"
        ) |>
        dplyr::rename(column = "gts_column")
    }

    # add the stats here to the header data frame
    x$table_styling$header <-
      x$table_styling$header |>
      dplyr::left_join(
        df_by_stats_wide,
        by = "column"
      ) |>
      tidyr::fill(any_of(c("modify_stat_N", "modify_stat_N_unweighted")), .direction = "updown")
  }

  # re-ording the columns
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::relocate(
      any_of(c("modify_stat_level",
               "modify_stat_N", "modify_stat_n", "modify_stat_p",
               "modify_stat_N_unweighted", "modify_stat_n_unweighted", "modify_stat_p_unweighted")),
      .before = last_col()
    )

  # return final object
  x
}
