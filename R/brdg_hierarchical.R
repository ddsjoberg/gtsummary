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
                              labels_hierarchy,
                              missing = "no",
                              missing_stat = "{N_miss}",
                              missing_text = "Unknown") {
  set_cli_abort_call()

  overall_stats <- cards |>
    dplyr::filter(is.na(gts_column))
  cards <- cards |>
    dplyr::filter(!is.na(gts_column))

  # will not need
  cards <- cards |>
    dplyr::filter(variable == tail(hierarchies, 1))

  # create groups for each hierarchy level combination
  x <- cards |>
    dplyr::group_by(dplyr::across(c(
      cards::all_ard_groups(types = "levels"),
      variable
    )))
  if (!is_empty(by)) {
    x <- x |> dplyr::ungroup(group1_level)
  }

  # build the table body pieces with bridge functions and stack them -----------
  sub_tbls <- x |>
    dplyr::group_map(
      function(.x, .y) {
        brdg_summary(
          cards = cards::bind_ard(
            .x |> cards::as_card(),
            overall_stats
          ),
          variables = .y$variable,
          type = type,
          statistic = statistic,
          by = by,
          missing = missing,
          missing_stat = missing_stat,
          missing_text = missing_test
        ) |>
          add_hierarchy_levels(.y)
      },
      .keep = TRUE
    )

  # order and stack hierarchy sub-tables
  x <- if (length(hierarchies) > 1) {
    .order_stack_sub_tables(sub_tbls)
  } else {
    sub_tbls[[1]]
  }

  # formulate top-left label for the label column
  indent <- unique(x$table_styling$indent$n_spaces)
  lbl_hierarch <- sapply(
    seq_along(labels_hierarchy),
    function(x) {
      paste0(
        paste(rep(" ", indent[x]), collapse = ""),
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
add_hierarchy_levels <- function(x, context) {
  # no hierarchy
  if (ncol(context) == 1) {
    # remove indent
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = row_type != "label",
        indent = 0
      )
    return(x)
  }

  context <- context |>
    dplyr::select(-variable)

  # add 'hierarchy' element to table_styling
  x$table_styling[["hierarchy"]] <- context

  labels <- context |>
    select(!cards::all_missing_columns()) |>
    unlist(use.names = FALSE) |>
    c()
  n_labels <- length(labels)
  pre_labels <- x$table_body |>
    dplyr::filter(row_type == "label") |>
    dplyr::pull(label)
  missing_labels <- setdiff(labels, pre_labels)

  if (length(missing_labels) > 0) {
    # add label rows for each additional hierarchy level
    x$table_body <-
      tibble(
        variable = x$table_body$variable[1],
        row_type = "label",
        var_label = missing_labels,
        label = missing_labels,
        var_type = "categorical"
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
      indent = n_labels * 4
    )

  x
}

.order_stack_sub_tables <- function(tbls) {
  ord_sub_tbls <-
    lapply(
      tbls,
      \(x) sapply(
        x$table_styling$hierarchy,
        \(x) {
          if (is.null(x[[1]])) " " else as.character(unlist(x))
        }
      )
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      idx = dplyr::cur_group_rows()
    ) |>
    dplyr::arrange(dplyr::across(-idx)) |>
    dplyr::pull(idx)

  tbl_stack(tbls[ord_sub_tbls], .condense = TRUE)
}
