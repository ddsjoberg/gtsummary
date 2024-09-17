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

  h <- unique(cards$variable)
  idx <- length(h)

  if (idx > 1) {
    cards_inner <- cards |>
      dplyr::filter(variable != h[1])

    x_inner <- brdg_hierarchical(
      cards = cards_inner,
      hierarchies = hierarchies,
      type = type,
      by = by,
      id = id,
      include = include,
      statistic = statistic,
      labels_hierarchy = labels_hierarchy,
      missing = missing,
      missing_stat = missing_stat,
      missing_text = missing_text
    )

    if (idx == length(hierarchies)) return(x_inner)
  } else {
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
    tbls <- x |>
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

    return(tbls)
  }

  # cards_outer <- cards[apply(cards, 1, \(x) !h[1] %in% unlist(x)), ]
  #
  # # create groups for each hierarchy level combination
  # x <- cards_outer |>
  #   dplyr::group_by(dplyr::across(c(
  #     cards::all_ard_groups(types = "levels"),
  #     variable
  #   )))
  # if (!is_empty(by)) {
  #   x <- x |> dplyr::ungroup(group1_level)
  # }
  #
  # this is what you would use as row labels
  # build the table body pieces with bridge functions and stack them -----------
  # x_outer <- x |>
  #   dplyr::group_map(
  #     function(.x, .y) {
  #       browser()
  #       brdg_summary(
  #         cards = cards::bind_ard(
  #           .x |> cards::as_card(),
  #           overall_stats
  #         ),
  #         variables = .y$variable,
  #         type = type,
  #         statistic = statistic,
  #         by = by,
  #         missing = missing,
  #         missing_stat = missing_stat,
  #         missing_text = missing_test
  #       ) |>
  #         add_hierarchy_levels(.y)
  #     },
  #     .keep = TRUE
  #   )

  # x_outer <- x |>
  #   dplyr::group_map(
  #     function(.x, .y) {
  #       brdg_summary(
  #         cards = cards::bind_ard(
  #           .x |> cards::as_card(),
  #           overall_stats
  #         ),
  #         variables = .y$variable,
  #         type = type,
  #         statistic = statistic,
  #         by = by,
  #         missing = missing,
  #         missing_stat = missing_stat,
  #         missing_text = missing_test
  #       ) |>
  #         add_hierarchy_levels(.y)
  #     },
  #     .keep = TRUE
  #   )

  # combine hierarchy sub-tables
  all_tbls <- x_inner#, x_outer)
  ord_all_tbls <- lapply(
    all_tbls,
    \(x) sapply(
      x$table_styling$hierarchy,
      \(x) {
        if (is.null(x[[1]])) NA else as.character(unlist(x))
      }
    )
  ) |>
    bind_rows() |>
    mutate(
      idx = dplyr::cur_group_rows()
    )

  ord_all_tbls <- ord_all_tbls |>
    tibble::add_column(
      checkna = !is.na(ord_all_tbls[ncol(ord_all_tbls) - 1]) |> unname(),
      .after = 1
    ) |>
    arrange(across(-idx)) |>
    dplyr::pull(idx)


  x <- tbl_stack(
    all_tbls[ord_all_tbls],
    .combine = TRUE
  ) |>
    add_hierarchy_levels(context = data.frame(hierarchies))

  return(x)

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
  # x <- x |>
  #   # updating the headers for the stats columns
  #   modify_header(
  #     label = lbl_hierarch,
  #     all_stat_cols() ~
  #       ifelse(
  #         is_empty(by),
  #         get_theme_element("tbl_summary-str:header-noby",
  #                           default = "**N = {style_number(N)}**"),
  #         get_theme_element("tbl_summary-str:header-withby",
  #                           default = "**{level}**  \nN = {style_number(n)}")
  #       )
  #   )

  # return tbl_summary table ---------------------------------------------------
  x$call_list <- list(tbl_summary = call)
  # running any additional mods
  x <-
    get_theme_element("tbl_summary-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}
