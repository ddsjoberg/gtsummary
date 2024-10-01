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

  overall_stats <- cards |>
    dplyr::filter(variable %in% by) |>
    mutate(gts_column = NA, context = "attributes")

  # process overall row data
  if (overall_row) {
    cards <- cards |>
      dplyr::mutate(
        variable_level = ifelse(variable == "..ard_hierarchical_overall..", label[["overall"]], variable_level),
        variable = ifelse(variable == "..ard_hierarchical_overall..", "overall", variable)
      )
    label[["overall"]] <- NULL
  }

  cards <- cards |> dplyr::filter(!variable %in% by)

  # create groups for each hierarchy level combination
  n_by <- length(by)
  by_groups <- (cards |> select(cards::all_ard_groups()) |> colnames())[seq_len(2 * length(by))]
  x <- cards |>
    dplyr::group_by(dplyr::across(c(
      cards::all_ard_groups(),
      cards::all_ard_variables("names"),
      -by_groups
    )))

  # build the table body pieces with bridge functions and stack them -----------
  sub_tbls <- x |>
    dplyr::group_map(
      function(.x, .y) {
        if (any(is.na(unlist(.y)))) {
          .x <- if (!.y$variable == "overall") {
            .x |>
            dplyr::group_by(across(c(
              cards::all_ard_groups(),
              cards::all_ard_variables(),
              -cards::all_missing_columns(),
              -by_groups
            )))
          } else {
            .x |> dplyr::group_by(across(cards::all_ard_variables()))
          }

          .x |>
            dplyr::group_map(
              function(.x, .y) {
                brdg_summary(
                  cards =
                    cards::bind_ard(
                      .x |> dplyr::bind_cols(.y) |> cards::as_card(),
                      overall_stats
                    ),
                  variables = .y$variable,
                  type = type,
                  statistic = statistic,
                  by = by
                ) |>
                  .add_hierarchy_levels(.y)
              }
            )
        } else {
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
            .add_hierarchy_levels(.y)
        }
      },
      .keep = TRUE
    )

  # order and stack hierarchy sub-tables
  if (length(sub_tbls) > 1) {
    x <- .order_stack_sub_tables(sub_tbls, setdiff(include, tail(hierarchies, 1)))
  } else {
    x <- sub_tbls[[1]]
  }

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

# add 'hierarchy' element to gtsummary object
.add_hierarchy_levels <- function(x, context) {
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

  if (all(c("variable", "variable_level") %in% names(context))) {
    x$table_body <- x$table_body |>
      dplyr::mutate(
        row_type = "label",
        var_label = label
      )
  } else {
    context <- context |>
      dplyr::select(-variable)
  }

  # add 'hierarchy' element to table_styling
  hierarchy_nms <- context[seq(1, ncol(context), 2)] |> unlist()
  hierarchy <- setNames(context[seq(2, ncol(context), 2)], hierarchy_nms)
  x$table_styling[["hierarchy"]] <- hierarchy[!is.na(names(hierarchy))]

  labels <- context |>
    select(cards::all_ard_groups("levels"), cards::all_ard_variables("levels")) |>
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

.order_stack_sub_tables <- function(tbls, include) {
  which_summary <- which(sapply(tbls, class) == "list")
  if (length(which_summary) > 0) {
    tbls <- c(
      tbls[which_summary] |> unlist(recursive = FALSE),
      tbls[-which_summary]
    )
  }

  ord_sub_tbls <- lapply(
    tbls,
    \(x) {
      x$table_styling$hierarchy |>
        dplyr::mutate(dplyr::across(where(is.list), ~unlist(.x)))
    }
  ) |>
    dplyr::bind_rows()

  ord_sub_tbls <- ord_sub_tbls |>
    dplyr::mutate(dplyr::across(everything(), .fns = ~tidyr::replace_na(., " "))) |>
    dplyr::mutate(idx = dplyr::cur_group_rows()) |>
    dplyr::arrange(dplyr::across(-idx)) |>
    dplyr::pull(idx)

  tbls <- tbls[ord_sub_tbls]
  attr(tbls, "include") <- include
  attr(tbls, "hierarchical") <- TRUE

  tbl_stack(tbls)
}
