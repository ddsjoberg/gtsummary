#' Hierarchical Summary Table Bridges
#'
#' @export
brdg_hierarchical <- function(cards,
                              hierarchies,
                              type,
                              by,
                              id,
                              include,
                              statistic,
                              cards_summary,
                              labels_hierarchy) {
  set_cli_abort_call()

  # browser()
  overall_stats <- cards |>
    dplyr::filter(!variable %in% c(hierarchies, "..ard_hierarchical_overall..")) |>
    mutate(gts_column = NA, context = "attributes")

  # process 'include'
  vars <- if (length(hierarchies) == 1) {
    hierarchies
  } else {
    which_include <- c(hierarchies %in% include, TRUE)
    c("..ard_hierarchical_overall..", hierarchies)[which_include]
  }

  cards <- cards |> dplyr::filter(group1 %in% by, variable %in% vars)
  cards$variable_level[cards$variable == "..ard_hierarchical_overall.."] <- list(NULL)

  overall_rows <- list()
  if (!is_empty(cards_summary)) {
    if (any(is.na(cards_summary$group1))) {
      x_overall <- cards_summary |>
        dplyr::filter(is.na(group1)) |>
        dplyr::group_by(variable_level) |>
        dplyr::mutate(
          variable = by,
          variable_level = "Total number of patients",
          gts_column = paste0("stat_", dplyr::cur_group_id())
        ) |>
        dplyr::ungroup()

      overall_rows <- list(
        brdg_summary(
          cards = x_overall |> cards::as_card(),
          variables = by,
          type = type,
          statistic = statistic |> setNames(by)
        ) |>
          add_hierarchy_levels(context = data.frame(variable = NA), summary_row = TRUE)
      )
    }

    if (any(!is.na(cards_summary$group1))) {
      by_groups <- (cards |> select(cards::all_ard_groups()) |> colnames())[seq_len(2 * length(by))]
      x_sum <- cards_summary |>
        dplyr::filter(!is.na(group1)) |>
        dplyr::group_by(dplyr::across(c(
          cards::all_ard_groups(),
          cards::all_ard_variables(),
          -by_groups
        )))

      overall_rows <- c(
        overall_rows,
        x_sum |>
          dplyr::group_map(
            function(.x, .y) {
              brdg_summary(
                cards =
                  cards::bind_ard(
                    .x |> cards::as_card(),
                    overall_stats
                  ),
                variables = .y$variable,
                type = type,
                statistic = statistic,
                by = by
              ) |>
                add_hierarchy_levels(.y, summary_row = TRUE)
            },
            .keep = TRUE
          )
      )
    }
  }

  if (length(hierarchies) == 1) {
    x <- brdg_summary(
      cards =
        cards::bind_ard(
          cards |> cards::as_card(),
          overall_stats
        ),
      variables = tail(hierarchies, 1),
      type = type,
      statistic = statistic,
      by = by
    ) |>
      add_hierarchy_levels(data.frame(variable = NA)) |>
      list()
  } else {
    # create groups for each hierarchy level combination
    n_by <- length(by) + as.numeric(length(hierarchies) > 1)
    by_groups <- (cards |> select(cards::all_ard_groups()) |> colnames())[seq_len(2 * n_by)]
    x <- cards |>
      dplyr::group_by(dplyr::across(c(
        cards::all_ard_groups(),
        cards::all_ard_variables(),
        -by_groups
      )))

    # build the table body pieces with bridge functions and stack them -----------
    sub_tbls <- x |>
      dplyr::group_map(
        function(.x, .y) {
          brdg_summary(
            cards =
              cards::bind_ard(
                .x |>
                  dplyr::rename(
                    !!paste0("group", n_by, "_level") := "variable_level",
                    !!paste0("group", n_by) := "variable",
                    "variable_level" = !!paste0("group", n_by, "_level"),
                    "variable" = !!paste0("group", n_by),
                  ) |>
                  cards::as_card(),
                overall_stats
              ),
            variables = tail(hierarchies, 1),
            type = type,
            statistic = statistic,
            by = by
          ) |>
            add_hierarchy_levels(.y)
        },
        .keep = TRUE
      )
  }

  # order and stack hierarchy sub-tables
  x <- .order_stack_sub_tables(c(overall_rows, sub_tbls))

  # formulate top-left label for the label column
  indent <- unique(x$table_styling$indent$n_spaces)[seq_along(hierarchies)]
  lbl_hierarch <- sapply(
    seq_along(labels_hierarchy),
    function(x) {
      paste0(
        paste(rep("\U00A0", indent[x]), collapse = ""),
        "**",
        labels_hierarchy[x],
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
      label = lbl_hierarch,
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

# add 'hierarchy' element to gtsummary object
add_hierarchy_levels <- function(x, context, summary_row = FALSE, count = FALSE) {
  # no hierarchy
  if (ncol(context) == 1 || context$variable == "..ard_hierarchical_overall..") {
    # remove indent
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = row_type != "label",
        indent = 0
      )
    return(x)
  }

  # add 'hierarchy' element to table_styling
  hierarchy_nms <- context[seq(1, ncol(context), 2)] |> unlist()
  hierarchy <- setNames(context[seq(2, ncol(context), 2)], hierarchy_nms)
  x$table_styling[["hierarchy"]] <- hierarchy[!is.na(names(hierarchy))]

  if (summary_row) {
    label_pos <- if (nrow(x$table_body) > 1) {
      max(which(x$table_body$row_type == "label"))
    } else {
      0
    }
    x$table_body <- x$table_body |>
      dplyr::add_row(
        variable = x$table_body$variable[label_pos + 1],
        var_type = "categorical",
        var_label = x$table_body$label[label_pos + 1],
        row_type = "label",
        label = x$table_body$label[label_pos + 1],
        .after = label_pos
      )
    x$table_body$label[label_pos + 2] <- if (count) "Total number of records" else "Total number of patients"

    # indent new label row
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = row_type == "label" & label == !!x$table_body$label[label_pos + 1],
        indent = label_pos * 4
      )
  }

  labels <- context |>
    select(cards::all_ard_groups("levels"), cards::all_ard_variables("levels")) |>
    unlist(use.names = FALSE) |>
    c()
  pre_labels <- x$table_body |>
    dplyr::filter(row_type == "label") |>
    dplyr::pull(label)
  missing_labels <- setdiff(labels, pre_labels)

  if (length(missing_labels) > 0) {
      # add label rows for each additional hierarchy level
      x$table_body <-
        tibble::tibble(
          variable = x$table_body$variable[1],
          var_type = "categorical",
          var_label = missing_labels,
          row_type = "label",
          label = missing_labels
        ) |>
        dplyr::bind_rows(x$table_body)

    # indent label rows for each hierarchy level
    for (i in seq_along(labels)) {
      x <- x |>
        modify_column_indent(
          columns = label,
          rows = row_type == "label" & var_label == !!labels[i],
          indent = (i - 1) * 4
        )
    }
  }

  # indent non-label rows
  x <- x |>
    modify_column_indent(
      columns = label,
      rows = row_type != "label",
      indent = length(labels) * 4
    )

  x
}

.order_stack_sub_tables <- function(tbls) {
  ord_sub_tbls <-
    lapply(
      tbls,
      \(x) {
        if (is.null(x$table_styling$hierarchy)) {
          data.frame(no_hierarchy = NA)
        } else {
          sapply(
            x$table_styling$hierarchy,
            \(x) if (is.na(x[[1]])) " " else as.character(unlist(x))
          )
        }
      }
    ) |>
    dplyr::bind_rows() |>
    select(-any_of("no_hierarchy")) |>
    mutate(across(everything(), ~replace(., is.na(.), " "))) |>
    dplyr::mutate(idx = dplyr::cur_group_rows()) |>
    dplyr::arrange(dplyr::across(-idx)) |>
    dplyr::pull(idx)

  tbl_stack(tbls[ord_sub_tbls], .condense = TRUE)
}
