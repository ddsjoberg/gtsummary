#' Hierarchy table bridge
#'
#' @description
#' Bridge function for converting `tbl_hierarchical()` (and similar) cards to basic gtsummary objects.
#'
#' @param cards (`card`)\cr
#'   an ARD object of class `"card"` created with `cards::ard_hierarchical_stack()`.
#' @param hierarchies (`character`)\cr
#'   character list of hierarchy variables.
#' @param by (`string`)\cr
#'   string indicating the stratifying column.
#' @param include (`character`)\cr
#'   character list of hierarchy variables to include summary statistics for.
#' @param statistic (named `list`)\cr
#'   named list of summary statistic names.
#' @param type (named `list`)\cr
#'   named list of summary types.
#' @param label (named `list`)\cr
#'   named list of hierarchy variable labels.
#'
#' @return a gtsummary object
#'
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#'
#' @export
brdg_hierarchical <- function(cards,
                              hierarchies,
                              by,
                              include,
                              statistic,
                              type,
                              overall_row,
                              label) {
  set_cli_abort_call()

  # process overall row data
  if (overall_row) {
    cards <- cards |>
      dplyr::mutate(
        variable_level = ifelse(variable == "..ard_hierarchical_overall..", label[["overall"]], variable_level),
        variable = ifelse(variable == "..ard_hierarchical_overall..", "overall", variable)
      )
    label[["overall"]] <- NULL
  }

  n_by <- length(by)
  by_groups <- (cards |> select(cards::all_ard_groups()) |> colnames())[seq_len(2 * length(by))]
  cards <- cards |>
    dplyr::ungroup() |>
    cards::as_card()

  if (overall_row) {
    over_row <- pier_summary_hierarchical(
      cards = cards,
      variables = "overall",
      hierarchies = hierarchies,
      statistic = statistic
    )
  }

  table_body <- list()
  for (i in seq_along(hierarchies)) {
    if (!hierarchies[i] %in% include) {
      group <- paste0("group", i + n_by)
      var <- (cards |>
        dplyr::select(group) |>
        tidyr::drop_na() |>
        dplyr::pull(group))[1]
      ordered_lvls <- cards |>
        dplyr::pull(paste0(group, "_level")) |>
        sapply(\(x) levels(as.factor(x))) |>
        unlist() |>
        unique()

      tbl_rows <- tibble::tibble(
        row_type = "level",
        var_label = ordered_lvls,
        variable = hierarchies[i],
        label = var_label,
        !!paste0("group", i + n_by) := var,
        !!paste0("group", i + n_by, "_level") := ordered_lvls,
      )

      if (i > 1) {
        tbl_rows <- right_join(
          table_body |>
            select(row_type, cards::all_ard_groups(), -any_of(c(group, paste0(group, "_level")))),
          tbl_rows,
          by = "row_type"
        )
      }
    } else {
      tbl_rows <- pier_summary_hierarchical(
        cards = cards,
        variables = hierarchies[i],
        hierarchies = hierarchies,
        statistic = statistic
      ) |>
        mutate(
          !!paste0("group", i + n_by) := as.character(variable),
          !!paste0("group", i + n_by, "_level") := as.character(label)
        )
    }

    if (all(is.na(tbl_rows$var_label))) tbl_rows <- tbl_rows |> dplyr::mutate(var_label = label)
    if (i > 1) {
      tbl_rows <- dplyr::bind_rows(
        table_body |> mutate(row_type = "label"),
        tbl_rows
      ) |>
        dplyr::mutate(dplyr::across(cards::all_ard_groups(), .fns = ~tidyr::replace_na(., " "))) |>
        dplyr::arrange(across(cards::all_ard_groups()))
    }
    table_body <- tbl_rows
  }

  if (overall_row) {
    table_body <- dplyr::bind_rows(
      over_row,
      table_body
    )
  }

  table_body <- table_body |>
    select(-cards::all_ard_groups())

  # construct default table_styling --------------------------------------------
  x <- .create_gtsummary_object(table_body)

  # add info to x$table_styling$header for dynamic headers ---------------------
  x <- .add_table_styling_stats(x, cards = cards, by = by, hierarchical = TRUE)

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
        .construct_hierarchical_footnote(cards, hierarchies, statistic, type)
    )

  x <- x |>
    structure(class = "gtsummary") |>
    modify_column_unhide(columns = all_stat_cols())

  # correct indentation to account for label rows
  for (i in seq_along(hierarchies)) {
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = variable == !!hierarchies[i],
        indent = (i - 1) * 4
      )
  }
  if (overall_row) {
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = variable == "overall",
        indent = 0
      )
  }

  # formulate top-left label for the label column
  indent <- 4 * (seq_along(hierarchies) - 1)
  label_hierarchy <- sapply(
    seq_along(label),
    function(x) {
      paste0(
        paste(rep("\U00A0", indent[x]), collapse = ""),
        "**",
        label[x],
        "**",
        if (x < length(indent)) "  "
      )
    }
  ) |>
    paste(collapse = "\n")

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(
      label = label_hierarchy,
      all_stat_cols() ~
        ifelse(
          is_empty(by),
          get_theme_element("tbl_summary-str:header-noby",
                            default = "**N = {style_number(N)}**"),
          get_theme_element("tbl_summary-str:header-withby",
                            default = "**{level}**  \nN = {style_number(n)}")
        )
    )

  # return tbl_summary table ---------------------------------------------------
  x$call_list <- list(tbl_summary = call)
  # running any additional mods
  x <-
    get_theme_element("tbl_summary-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}

#' @rdname brdg_hierarchical
#' @export
pier_summary_hierarchical <- function(cards,
                                      variables,
                                      hierarchies,
                                      statistic) {
  set_cli_abort_call()
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }
  by <- setdiff(cards$group1, hierarchies)
  by <- by[!is.na(by)]
  by_cols <- (cards |> select(cards::all_ard_groups()) |> colnames())[seq_len(2 * length(by))]

  # subsetting cards object on categorical summaries ----------------------------
  cards_no_attr <-
    cards |>
    dplyr::filter(.data$variable %in% .env$variables, !.data$context %in% "attributes") |>
    cards::apply_fmt_fn() |>
    dplyr::mutate(across(
      cards::all_ard_groups("levels"),
      \(x) if (!is.null(x[[1]])) unlist(x) else NA_character_
    ))

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
            dplyr::bind_cols(
              df_groups_and_variable |>
                select(cards::all_ard_groups(), -by_cols)
            ) |>
            dplyr::group_by(variable_level) |>
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
                  label = .data$variable_level |> unlist()
                ) |>
                  bind_cols(
                    df_variable_level_stats[1, ] |> select(cards::all_ard_groups())
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
    )

  # for hierarchical tables, manually add 'var_label'
  df_result_levels <- df_result_levels |>
    dplyr::mutate(var_label = NA_character_)

  df_last <- df_result_levels |>
    select(cards::all_ard_groups("levels")) |>
    dplyr::pull(dplyr::last_col()) |>
    unlist()
  if (cards |> select(cards::all_ard_groups("names")) |> ncol() > 1 && !is.null(unlist(df_last))) {
    df_result_levels$var_label <- df_last
  }

  df_result_levels <-
    df_result_levels |>
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
      id_cols = c("row_type", "var_label", "variable", "label", cards::all_ard_groups()),
      names_from = "gts_column",
      values_from = "stat"
    )

  df_result_levels
}

.construct_hierarchical_footnote <- function(card, include, statistic, type) {
  include |>
    lapply(
      function(variable) {
        card |>
          dplyr::filter(.data$variable %in% .env$include) |>
          dplyr::select("stat_name", "stat_label") |>
          dplyr::distinct() %>%
          {stats::setNames(as.list(.$stat_label), .$stat_name)} |> # styler: off
          glue::glue_data(
            gsub("\\{(p|p_miss|p_nonmiss|p_unweighted)\\}%", "{\\1}", x = statistic[[variable]])
          )
      }
    ) |>
    stats::setNames(include) |>
    compact() |>
    unlist() |>
    unique() %>%
    {switch(!is.null(.), paste(., collapse = "; "))} # styler: off
}
