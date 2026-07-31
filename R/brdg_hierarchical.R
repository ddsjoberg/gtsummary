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
    cards::as_card(check = FALSE)

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
  excl <- which(!variables %in% include)
  if (length(excl) > 0L) {
    all_gps <- table_body |> select(cards::all_ard_groups("names")) |> names()
    ord <- utils::head(c(rbind(paste0(all_gps, "_level"), all_gps)), -1)

    # complete-path rows are the invariant source for every depth's label rows:
    # rows carrying a value in every group-name column. (Label rows added below
    # would carry " "/NA in deeper group columns, so they are never a source --
    # which is why a single pass reproduces the former per-variable loop.)
    complete_rows <- table_body |>
      dplyr::filter(!dplyr::if_any(all_of(all_gps), ~ is.na(.x) | .x == " "))

    lst_dummy <- lapply(excl, function(i) {
      prior_gp <- paste0("group", 1:i + n_by)
      prior_gp_lvl <- paste0(prior_gp, "_level")
      groupX <- dplyr::last(prior_gp)
      groupX_lvl <- dplyr::last(prior_gp_lvl)

      complete_rows |>
        select(all_of(c("row_type", prior_gp, prior_gp_lvl))) |>
        unique() |>
        mutate(
          var_label = .data[[groupX_lvl]],
          variable = .data[[groupX]],
          label = .data[[groupX_lvl]],
          row_type = "label"
        )
    })

    table_body <-
      rlang::inject(dplyr::bind_rows(table_body, !!!lst_dummy)) |>
      mutate(across(cards::all_ard_groups(), .fns = ~tidyr::replace_na(., " "))) |>
      dplyr::arrange(across(all_of(c(ord, "var_label"))))
  }

  if (overall_row) {
    table_body <- vctrs::vec_rbind(over_row, table_body)
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
      modify_indent(
        columns = label,
        rows = .data$variable == !!variables[i],
        indent = (i - 1) * 4
      )
  }
  if (overall_row) {
    x <- x |>
      modify_indent(
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
        label[variables][x],
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
                            default = "**Overall**"),
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
    dplyr::filter(.data$variable %in% .env$variables, !.data$context %in% "attributes")

  # both internal callers (internal_tbl_hierarchical() and tbl_ard_hierarchical())
  # already apply formatting functions before reaching this function. Only re-apply
  # when a cell actually needs it, which exactly reproduces `apply_fmt_fun(replace = FALSE)`.
  if (!"stat_fmt" %in% names(cards_no_attr) ||
    any(vapply(cards_no_attr$stat_fmt, is.null, logical(1)) &
      vapply(cards_no_attr$fmt_fun, Negate(is.null), logical(1)))) {
    cards_no_attr <- cards::apply_fmt_fun(cards_no_attr)
  }
  cards_no_attr <- cards_no_attr |> mutate(sort_idx = dplyr::row_number())

  # construct formatted statistics ---------------------------------------------
  # Vectorize the glue interpolation: pivot the ARD wide (one column per stat_name)
  # and glue once per variable, rather than looping group-by-group. Rows with a
  # populated `variable_level` are the per-level stats (one table row each); rows
  # with a NULL `variable_level` are variable-scope stats that glue appends to
  # every level of the group, with the variable-scope stat winning on any name
  # collision (matching the prior `c(level_stats, variable_stats)` order, which
  # under `glue_data()` resolves duplicate names last-wins).
  group_cols <- cards_no_attr |> select(cards::all_ard_groups()) |> colnames()
  hier_group_cols <- setdiff(group_cols, by_cols)
  stat_cols <- unique(cards_no_attr$stat_name)

  is_level <- !map_lgl(cards_no_attr$variable_level, is.null)
  level_rows <- cards_no_attr[is_level, , drop = FALSE]
  # per-level sort key = smallest stat-row index within the level (equivalent to
  # the prior `df_variable_level_stats$sort_idx[1]`, since row order is preserved)
  gid <- vctrs::vec_group_id(level_rows[c("gts_column", group_cols, "variable", "variable_level")])
  level_rows$sort_idx <- stats::ave(level_rows$sort_idx, gid, FUN = min)
  # level label, combining the length-1 `variable_level` cells to their common type
  # (e.g. factor + character levels across variables -> character), matching the
  # prior per-group `unlist()` followed by a cross-variable row-bind
  level_rows$label <- vctrs::list_unchop(level_rows$variable_level)

  level_wide <-
    tidyr::pivot_wider(
      level_rows,
      id_cols = c("variable", "gts_column", all_of(group_cols), "label", "sort_idx"),
      names_from = "stat_name",
      values_from = "stat_fmt",
      values_fn = list
    )
  # unlist only the wide stat columns; the group `*_level` id columns must remain
  # list columns for the downstream unnest
  sc <- intersect(stat_cols, names(level_wide))
  level_wide[sc] <- .unlist_wide_stat_cols(level_wide[sc])

  # merge variable-scope stats onto each level of the same group; variable-scope wins
  var_rows <- cards_no_attr[!is_level, , drop = FALSE]
  if (nrow(var_rows) > 0L) {
    key <- c("variable", "gts_column", group_cols)
    var_wide <-
      tidyr::pivot_wider(
        var_rows,
        id_cols = all_of(key),
        names_from = "stat_name",
        values_from = "stat_fmt",
        values_fn = list
      )
    var_sc <- intersect(stat_cols, names(var_wide))
    var_wide[var_sc] <- .unlist_wide_stat_cols(var_wide[var_sc])
    # presence indicator: a variable-scope stat overrides only where its ARD row exists
    var_present <-
      tidyr::pivot_wider(
        var_rows |> mutate(..present.. = TRUE),
        id_cols = all_of(key),
        names_from = "stat_name",
        values_from = "..present..",
        values_fn = function(x) TRUE,
        values_fill = FALSE
      )
    idx <- vctrs::vec_match(level_wide[key], var_wide[key])
    for (col in setdiff(names(var_wide), key)) {
      has <- !is.na(idx) & col %in% names(var_present) & var_present[[col]][idx]
      has[is.na(has)] <- FALSE
      if (!col %in% names(level_wide)) level_wide[[col]] <- NA
      level_wide[[col]][has] <- var_wide[[col]][idx[has]]
    }
  }

  # evaluate the glue statistic per variable (vectorized over that variable's levels)
  df_glued <-
    lapply(
      variables,
      function(var) {
        df_var <- level_wide[level_wide$variable == var, , drop = FALSE]
        if (nrow(df_var) == 0L) {
          return(NULL)
        }
        keep_cols <- setdiff(names(df_var), stat_cols)
        rlang::inject(vctrs::vec_rbind(!!!lapply(
          statistic[[var]],
          function(str_to_glue) {
            out <- df_var[, keep_cols, drop = FALSE]
            out$stat <- as.character(glue::glue_data(df_var, str_to_glue))
            out
          }
        )))
      }
    ) |>
    (function(lst) rlang::inject(vctrs::vec_rbind(!!!lst)))() %>%
    # this ensures the correct order when there are 10+ hierarchy levels
    dplyr::left_join(
      cards_no_attr |> dplyr::distinct(!!sym("gts_column")),
      .,
      by = "gts_column"
    )

  # reshape results for final table --------------------------------------------
  df_result_levels <-
    df_glued |>
    mutate(row_type = "level", var_label = NA_character_) |>
    select(
      "row_type", "var_label", "variable", "label",
      all_of(hier_group_cols), "gts_column", "stat", "sort_idx"
    ) |>
    dplyr::arrange(.data$sort_idx) |>
    tidyr::pivot_wider(
      id_cols = c("row_type", "var_label", "variable", "label", cards::all_ard_groups()),
      names_from = "gts_column",
      values_from = "stat"
    )

  # if overall_row present, change TRUE to NULL in applicable rows for compatibility when unnesting
  last_gp <- df_result_levels |> select(cards::all_ard_groups("names")) |> names() |> dplyr::last()
  if (!is.na(last_gp) && "..ard_hierarchical_overall.." %in% df_result_levels[[last_gp]]) {
    idx_overall <- which(df_result_levels[[last_gp]] == "..ard_hierarchical_overall..")
    df_result_levels[[paste0(last_gp, "_level")]][idx_overall] <- list(NULL)
  }

  df_result_levels <- df_result_levels |>
    tidyr::unnest(cols = cards::all_ard_groups("levels"), keep_empty = TRUE) |>
    mutate(across(where(is.factor), as.character))

  if (length(variables) > 1 && length(include) > 1) {
    gps <- df_result_levels |> select(cards::all_ard_groups("names")) |> names()

    # per-column subset assignment (avoids copying the whole frame each iteration)
    for (i in seq_along(gps)) {
      idx <- which(df_result_levels$variable == variables[i])
      if (length(idx) == 0L) next
      gp <- gps[i]
      gp_lvl <- paste0(gp, "_level")
      df_result_levels[[gp]][idx] <-
        dplyr::coalesce(df_result_levels[[gp]][idx], df_result_levels$variable[idx])
      df_result_levels[[gp_lvl]][idx] <-
        dplyr::coalesce(df_result_levels[[gp_lvl]][idx], df_result_levels$label[idx])
    }
  }

  df_result_levels
}

.construct_hierarchical_footnote <- function(card, include, statistic) {
  # the stat_name -> stat_label lookup does not depend on the loop variable,
  # so compute it once instead of re-filtering the full ARD for each variable
  lst_labels <- card |>
    dplyr::filter(.data$variable %in% .env$include) |>
    select("stat_name", "stat_label") |>
    dplyr::distinct() %>%
    {stats::setNames(as.list(.$stat_label), .$stat_name)} # styler: off

  include |>
    lapply(
      function(variable) {
        glue::glue_data(
          lst_labels,
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
