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
                              overall_row,
                              labels_hierarchy) {
  set_cli_abort_call()

  browser()
  overall_stats <- cards %>% dplyr::filter(!variable %in% c(hierarchies, "..ard_hierarchical_overall.."))
  cards <- cards %>% dplyr::filter(variable %in% c(hierarchies, "..ard_hierarchical_overall.."))
  cards$variable_level[cards$variable == "..ard_hierarchical_overall.."] <- list(NULL)

  # create groups for each hierarchy level combination
  x <- cards |>
    dplyr::group_by(dplyr::across(c(
      cards::all_ard_groups(),
      cards::all_ard_variables()
    )))
  if (!is_empty(by)) {
    x <- x |> dplyr::ungroup(group1, group1_level, group2, group2_level)
  } else {
    x <- x |> dplyr::ungroup(group1, group1_level)
  }

  # browser()

  # build the table body pieces with bridge functions and stack them -----------
  sub_tbls <- x |>
    dplyr::group_map(
      function(.x, .y) {
        brdg_summary(
          cards = #.x |> cards::as_card(),
            cards::bind_ard(
            .x |>
              dplyr::rename(
                "group2_level" = "variable_level",
                "group2" = "variable",
                "variable_level" = "group2_level",
                "variable" = "group2",
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

  browser()

  # order and stack hierarchy sub-tables
  x <- if (length(hierarchies) > 1) {
    .order_stack_sub_tables(sub_tbls)
  } else {
    sub_tbls[[1]]
  }

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
add_hierarchy_levels <- function(x, context) {
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

  # browser()
  # context <- context |>
  #   dplyr::select(-variable)

  labels <- context |>
    select(cards::all_ard_groups("levels"), cards::all_ard_variables("levels")) |>
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
      tibble::tibble(
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
    select(-no_hierarchy) |>
    mutate(across(everything(), ~replace(., is.na(.), " "))) |>
    dplyr::mutate(
      idx = dplyr::cur_group_rows()
    ) |>
    dplyr::arrange(dplyr::across(-idx)) |>
    dplyr::pull(idx)

  tbl_stack(tbls[ord_sub_tbls], .condense = TRUE)
}
