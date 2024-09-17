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

  # combine hierarchy sub-tables
  ord_sub_tbls <-
    lapply(
      sub_tbls,
      \(x) sapply(
        x$table_styling$hierarchy,
        \(x) {
          if (is.null(x[[1]])) " " else as.character(unlist(x))
        }
      )
    ) |>
    bind_rows() |>
    mutate(
      idx = dplyr::cur_group_rows()
    ) |>
    arrange(across(-idx)) |>
    dplyr::pull(idx)

  x <- tbl_stack(sub_tbls[ord_sub_tbls], .combine = TRUE)

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
