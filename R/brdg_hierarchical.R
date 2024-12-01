#' Hierarchy table bridge
#'
#' @description
#' Bridge function for converting `tbl_hierarchical()` (and similar) cards to basic gtsummary objects.
#' All bridge functions begin with prefix `brdg_*()`.
#'
#' This file also contains helper functions for constructing the bridge,
#' referred to as the piers (supports for a bridge) and begin with `pier_*()`.
#'
#' - `brdg_hierarchical()`: The bridge function ingests an ARD data frame and returns
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
#'   added in `brdg_hierarchical()`.
#'
#' @param cards (`card`)\cr
#'   an ARD object of class `"card"` created with `cards::ard_hierarchical_stack()`.
#' @param variables (`character`)\cr
#'   character list of hierarchy variables.
#' @param by (`string`)\cr
#'   string indicating the stratifying column.
#' @param include (`character`)\cr
#'   character list of hierarchy variables to include summary statistics for.
#' @param statistic (named `list`)\cr
#'   named list of summary statistic names.
#' @param count (scalar `logical`)\cr
#'   whether `tbl_hierarchical_count()` (`TRUE`) or `tbl_hierarchical()` (`FALSE`) is being applied.
#' @param is_ordered (scalar `logical`)\cr
#'   whether the last variable in `variables` is ordered.
#' @param label (named `list`)\cr
#'   named list of hierarchy variable labels.
#' @inheritParams tbl_hierarchical
#'
#' @return a gtsummary object
#'
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#'
#' @export
brdg_hierarchical <- function(cards,
                              variables,
                              by,
                              include,
                              statistic,
                              overall_row,
                              count,
                              is_ordered,
                              label) {
  set_cli_abort_call()

  # process overall row data
  if (overall_row) {
    cards <- cards |>
      mutate(
        variable_level = ifelse(
          .data$variable == "..ard_hierarchical_overall..", label[["..ard_hierarchical_overall.."]], .data$variable_level
        ),
      )
  }

  n_by <- length(by)
  by_groups <- (cards |> select(cards::all_ard_groups()) |> colnames())[seq_len(2 * length(by))]
  cards <- cards |>
    dplyr::ungroup() |>
    cards::as_card()

  if (overall_row) {
    over_row <- pier_summary_hierarchical(
      cards = cards,
      variables = "..ard_hierarchical_overall..",
      include = include,
      statistic = statistic
    )
  }

  table_body <- pier_summary_hierarchical(
    cards = cards,
    variables = variables,
    include = include,
    statistic = statistic
  )

  # add label rows for variables not in 'include'
  for (i in which(!variables %in% include)) {
    prior_gp <- paste0("group", 1:i + n_by)
    prior_gp_lvl <- paste0(prior_gp, "_level")
    groupX <- dplyr::last(prior_gp)
    groupX_lvl <- dplyr::last(prior_gp_lvl)

    # create dummy rows
    tbl_rows <- table_body |>
      dplyr::filter(!dplyr::if_any(cards::all_ard_groups("names"), ~ .x == " ")) |>
      select(all_of(c("row_type", prior_gp, prior_gp_lvl))) |>
      unique() |>
      mutate(
        var_label = .data[[groupX_lvl]],
        variable = .data[[groupX]],
        label = .data[[groupX_lvl]]
      )

    all_gps <- table_body |> select(cards::all_ard_groups("names")) |> names()
    ord <- utils::head(c(rbind(paste0(all_gps, "_level"), all_gps)), -1)

    tbl_rows <- dplyr::bind_rows(
      table_body,
      tbl_rows |> mutate(row_type = "label")
    ) |>
      mutate(across(cards::all_ard_groups(), .fns = ~tidyr::replace_na(., " "))) |>
      dplyr::group_by(across(cards::all_ard_groups("levels"))) |>
      dplyr::arrange(across(all_of(c(ord, "var_label")))) |>
      dplyr::ungroup()

    table_body <- tbl_rows
  }

  if (overall_row) {
    table_body <- dplyr::bind_rows(over_row, table_body)
  }

  # add hierarchy levels to table_body for sorting & filtering -----------------
  table_body <- table_body |>
    dplyr::relocate(cards::all_ard_groups(), .after = "row_type") |>
    mutate(across(cards::all_ard_groups(), .fns = ~str_replace(., "^ $", NA)))
  if (n_by > 0 && length(variables) > 1) {
    which_gps <- which(names(table_body) %in% (table_body |> select(cards::all_ard_groups()) |> names()))
    if (n_by > 0) {
      names(table_body)[which_gps] <- sapply(
        names(table_body)[which_gps],
        function(x) {
          n <- as.numeric(gsub(".*([0-9]+).*", "\\1", x)) - n_by
          gsub("[0-9]+", n, x)
        }
      )
    }
    for (i in which_gps[c(TRUE, FALSE)]) {
      lbl_row <- which(is.na(table_body[i]) & !is.na(table_body[i + 1]))
      table_body[lbl_row, i] <- table_body$variable[lbl_row]
    }
  }
  if (overall_row && "group1" %in% names(table_body)) {
    table_body$group1[table_body$variable == "..ard_hierarchical_overall.."] <- "..ard_hierarchical_overall.."
  }

  # construct default table_styling --------------------------------------------
  x <- .create_gtsummary_object(table_body)

  # add info to x$table_styling$header for dynamic headers ---------------------
  noby_groups <- cards |> select(cards::all_ard_groups()) |> names() |> setdiff(by_groups)
  x <- .add_table_styling_stats(x, cards = cards |> select(-all_of(noby_groups)), by = by)

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
        .construct_hierarchical_footnote(cards, variables, statistic)
    )

  x <- x |>
    structure(class = "gtsummary") |>
    modify_column_unhide(columns = all_stat_cols())

  # correct indentation to account for label rows
  for (i in seq_along(variables)) {
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = .data$variable == !!variables[i],
        indent = (i - 1) * 4
      )
  }
  if (overall_row) {
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = .data$variable == "..ard_hierarchical_overall..",
        indent = 0
      )
  }

  # formulate top-left label for the label column
  indent <- 4 * (seq_along(variables) - 1)
  label_hierarchy <- sapply(
    seq_along(label[variables]),
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
        dplyr::case_when(
          is_empty(by) && "modify_stat_N" %in% names(x$table_styling$header) ~
            get_theme_element("tbl_hierarchical-str:header-noby",
                            default = "**N = {style_number(N)}**"),
          is_empty(by) ~
            get_theme_element("tbl_hierarchical-str:header-noby-noN",
                            default = "Overall"),
          "modify_stat_n" %in% names(x$table_styling$header) ~
            get_theme_element("tbl_hierarchical-str:header-withby",
                            default = "**{level}**  \nN = {style_number(n)}"),
          !"modify_stat_n" %in% names(x$table_styling$header) ~
            get_theme_element("tbl_hierarchical-str:header-withby-noN",
                              default = "**{level}**")
        )
    )

  # return tbl_hierarchical table ---------------------------------------------------
  x$call_list <- list(call) |>
    stats::setNames(if (count) "tbl_hierarchical_count" else "tbl_hierarchical")
  # running any additional mods
  x <-
    get_theme_element("tbl_hierarchical-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}

#' @rdname brdg_hierarchical
#' @export
pier_summary_hierarchical <- function(cards,
                                      variables,
                                      include,
                                      statistic) {
  set_cli_abort_call()
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }

  # identify 'by' groups
  by <- setdiff(cards[["group1"]], variables)
  by <- by[!is.na(by)]
  by_cols <- (cards |> select(cards::all_ard_groups()) |> colnames())[seq_len(2 * length(by))]

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

        mutate(
          .data = df_groups_and_variable,
          df_stats =
            dplyr::filter(df_variable_stats, !.data$variable_level %in% list(NULL)) |>
            dplyr::bind_cols(
              df_groups_and_variable |>
                select(cards::all_ard_groups(), -all_of(by_cols))
            ) |>
            dplyr::group_by(.data$variable_level) |>
            dplyr::group_map(
              function(df_variable_level_stats, df_variable_levels) {
                mutate(
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
                  dplyr::bind_cols(
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
    # this ensures the correct order when there are 10+ hierarchy levels
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
        select("variable", var_label = "stat"),
      by = "variable"
    )

  # add var_label
  df_result_levels <- df_result_levels |> mutate(var_label = NA_character_)

  df_result_levels <-
    df_result_levels |>
    mutate(
      .by = "variable",
      row_type = "level",
      var_label = unlist(.data$var_label),
      .after = 0L
    ) |>
    select(-cards::all_ard_groups()) |>
    tidyr::unnest(cols = "df_stats") |>
    tidyr::unnest(cols = "stat") |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label", cards::all_ard_groups()),
      names_from = "gts_column",
      values_from = "stat"
    ) |>
    tidyr::unnest(cols = cards::all_ard_groups("levels"), keep_empty = TRUE) |>
    mutate(across(where(is.factor), as.character))

  if (length(variables) > 1 && length(include) > 1) {
    gps <- df_result_levels |> select(cards::all_ard_groups("names")) |> names()

    df_result_levels <- df_result_levels |>
      mutate(across(cards::all_ard_groups("names"), .fns = ~tidyr::replace_na(., " ")))

    for (i in seq_along(gps)) {
      df_result_levels[df_result_levels$variable == variables[i], ] <-
        df_result_levels[df_result_levels$variable == variables[i], ] |>
        mutate(!!paste0(gps[i], "_level") := dplyr::coalesce(!!sym(paste0(gps[i], "_level")), .data$label))
    }
    ord <- c(rbind(paste0(gps, "_level"), gps))
    df_result_levels <- df_result_levels |>
      dplyr::group_by(across(cards::all_ard_groups("levels"))) |>
      dplyr::arrange(across(all_of(ord))) |>
      dplyr::ungroup()
  }

  df_result_levels
}

.construct_hierarchical_footnote <- function(card, include, statistic) {
  include |>
    lapply(
      function(variable) {
        card |>
          dplyr::filter(.data$variable %in% .env$include) |>
          select("stat_name", "stat_label") |>
          dplyr::distinct() %>%
          {stats::setNames(as.list(.$stat_label), .$stat_name)} |> # styler: off
          glue::glue_data(
            gsub("\\{(p)\\}%", "{\\1}", x = statistic[[variable]])
          )
      }
    ) |>
    stats::setNames(include) |>
    compact() |>
    unlist() |>
    unique() %>%
    {switch(!is.null(.), paste(., collapse = "; "))} # styler: off
}
