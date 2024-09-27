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
#'   character list of hierarchy variables to include statistics for.
#' @param statistic (named `list`)\cr
#'   named list of summary statistic names.
#' @param type (named `list`)\cr
#'   named list of summary types.
#' @param cards_summary (`card`)\cr
#'   an ARD object of class `"card"` created with `cards::ard_hierarchical_stack()`.
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
                              cards_summary,
                              label) {
  set_cli_abort_call()

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

  # add summary rows if requested -----------------------------------
  overall_rows <- list()
  if (!is_empty(cards_summary)) {
    if (any(is.na(cards_summary$group1))) {
      x_overall <- cards_summary |>
        dplyr::filter(is.na(group1)) |>
        dplyr::group_by(variable_level) |>
        dplyr::mutate(
          variable = by,
          variable_level = list("Total number of patients"),
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
          .add_hierarchy_levels(context = data.frame(variable = NA), summary_row = TRUE)
      )

      # add column counts to header df
      nrow_header <- nrow(overall_rows[[1]]$table_styling$header)
      N <- overall_stats$stat[overall_stats$stat_name == "N"][[1]]
      n_trt <- overall_stats$stat[overall_stats$stat_name == "n"] |> unlist()
      overall_rows[[1]]$table_styling$header <- overall_rows[[1]]$table_styling$header |>
        dplyr::mutate(
          modify_stat_level = c(rep(NA, nrow_header - length(n_trt)), unique(unlist(cards_summary$variable_level))),
          modify_stat_N = rep(N, nrow_header),
          modify_stat_n = c(rep(NA, nrow_header - length(n_trt)), n_trt),
          modify_stat_p = modify_stat_n / modify_stat_N
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
                .add_hierarchy_levels(.y, summary_row = TRUE)
            },
            .keep = TRUE
          )
      )
    }
  }

  # calculate sub-tables ----------------------------
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
      .add_hierarchy_levels(data.frame(variable = NA)) |>
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
            .add_hierarchy_levels(.y)
        },
        .keep = TRUE
      )
  }

  # order and stack hierarchy sub-tables
  x <- .order_stack_sub_tables(c(overall_rows, sub_tbls))

  # formulate top-left label for the label column
  indent <- unique(x$table_styling$indent$n_spaces)[seq_along(hierarchies)]
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
    ) |>
    modify_table_styling(

    )

  # return tbl_summary table ---------------------------------------------------
  x$call_list <- list(tbl_summary = call)
  # running any additional mods
  x <-
    get_theme_element("tbl_summary-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}

# add 'hierarchy' element to gtsummary object, correct labelling and indentation
.add_hierarchy_levels <- function(x, context, summary_row = FALSE, count = FALSE) {
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

  # extract and identify missing labels
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
