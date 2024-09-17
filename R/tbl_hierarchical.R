#' Create hierarchical table
#'
#' @examples
#' data <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:5]
#'   )
#'
#' tbl_hierarchical(
#'   data = data,
#'   hierarchies = c(AESOC, AETERM, AESEV),
#'   by = TRTA,
#'   denominator = cards::ADSL,
#'   id = USUBJID
#' )
#'
#' @export
tbl_hierarchical <- function(data,
                             hierarchies,
                             by = NULL,
                             id = NULL,
                             label = NULL,
                             denominator = NULL,
                             include = everything(), # this would be the variables from `hierarchy` that we would include summary stats for (some of the nested drug class tables don't need stats on the class level)
                             statistic = ifelse(!missing(id), "{n} ({p})", "{n}"),
                             digits = NULL,
                             overall_row = FALSE) {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(hierarchies)
  check_string(statistic)
  check_scalar_logical(overall_row)

  # evaluate tidyselect
  cards::process_selectors(data, hierarchies = {{ hierarchies }}, id = {{ id }}, by = {{ by }})
  cards::process_selectors(data[hierarchies], include = {{ include }})
  # if id provided, then check that denominator also provided
  if (!is_empty(id)) {
    check_data_frame(
      denominator,
      message = "A {.cls data.frame} must be passed in argument {.arg denominator} when argument {.arg id} is supplied."
    )
  }

  if (is_empty(id) + is_empty(denominator) == 1L) {
    cli::cli_abort(
      "Specify both arguments {.arg id} and {.arg denominator}, or neither.",
      call = get_cli_abort_call()
    )
  }

  # check that neither 'hierarchies' nor 'include' is empty
  if (is_empty(hierarchies) || is_empty(include)) {
    cli::cli_abort(
      message = "Arguments {.arg hierarchies} and {.arg include} cannot be empty.",
      call = get_cli_abort_call()
    )
  }

  # check that 'include' is the correct subset of 'hierarchies'
  if (!setequal(include, hierarchies[seq_len(length(include))])) {
    cli::cli_abort(
      message = c("The columns selected in {.arg include} must be nested within the columns in {.arg hierarchies}",
                  "i" = "For example, when {.code hierarchies = c(SOC, AETERM)}, {.arg include} can be {.code AETERM} but not {.code SOC}.")
    )
  }

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())

  # get variable labels for each hierarchy level
  labels_hierarchy <- sapply(hierarchies, \(x) if (!is.na(attr(data[[x]], "label"))) attr(data[[x]], "label") else x)

  type <- assign_summary_type(data, include, value = NULL)

  statistic <- as.formula(sprintf("all_categorical() ~ \"%s\"", statistic))

  # calculate statistics -------------------------------------------------------
  # TODO: Update this with cards::ard_stack_hierarchical() when it's ready
  cards <-
    ard_stack_hierarchical(
      data = data,
      hierarchies = all_of(hierarchies),
      by = all_of(by),
      statistic = statistic,
      denominator = denominator,
      id = all_of(id),
      include = include
    )

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic =
      case_switch(
        missing(statistic) ~ get_theme_element("tbl_summary-arg:statistic", default = statistic),
        .default = statistic
      ),
    include_env = TRUE
  )

  # add the calling env to the statistics
  statistic <- .add_env_to_list_elements(statistic, env = caller_env())

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    label =
      case_switch(
        missing(label) ~ get_deprecated_theme_element("tbl_summary-arg:label", default = label),
        .default = label
      )
  )

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    digits =
      case_switch(
        missing(digits) ~ get_theme_element("tbl_summary-arg:digits", default = digits),
        .default = digits
      )
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic =
      get_theme_element("tbl_summary-arg:statistic", default = eval(formals(gtsummary::tbl_summary)[["statistic"]])),
    digits =
      get_theme_element("tbl_summary-arg:digits", default = eval(formals(gtsummary::tbl_summary)[["digits"]]))
  )

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <-
      scope_table_body(.list2tb(type, "var_type"), data[include]) |>
      assign_summary_digits(statistic, type, digits = digits)
  }

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_summary(cards, hierarchies, by, hierarchical = TRUE)

  # add total N rows to 'cards'
  cards <- cards::bind_ard(
    cards,
    cards::ard_total_n(data = data) |>
      dplyr::mutate(gts_column = NA),
    case_switch(
      !is_empty(by) ~
        cards::ard_categorical(
          data,
          variables = dplyr::all_of(by),
          stat_label = ~ default_stat_labels()
        ) |>
        dplyr::mutate(gts_column = NA),
      .default = NULL
    )
  )

  # call bridge function here
  brdg_hierarchical(
    cards,
    hierarchies,
    type,
    by,
    id,
    include,
    statistic,
    labels_hierarchy
  ) |>
    append(
      list(
        cards = list(tbl_hierarchical = cards),
        inputs = tbl_hierarchical_inputs
      )
    ) |>
    structure(class = c("tbl_hierarchical", "gtsummary"))
}

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

  # # create groups for each hierarchy level combination
  # x <- cards |>
  #   dplyr::group_by(dplyr::across(c(
  #     cards::all_ard_groups(types = "levels"),
  #     variable
  #   )))
  # if (!is_empty(by)) {
  #   x <- x |> dplyr::ungroup(group1_level)
  # }
  # x_grps <- as.data.frame(group_keys(x))[1] |> unique()
  # x_last <- x |>
  #   ungroup() |>
  #   select(cards::all_ard_groups("levels")) |>
  #   select(last_col()) |>
  #   names()

  ards <- list()
  h <- unique(cards$variable)
  idx <- length(h)
  browser()

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
    x_grps <- as.data.frame(group_keys(x))[1] |> unique()
    x_last <- x |>
      ungroup() |>
      select(cards::all_ard_groups("levels")) |>
      select(last_col()) |>
      names()
    # browser()
    ards <- list()

    # build the table body pieces with bridge functions and stack them -----------
    for (lvl in unlist(x_grps)) {
      x_f <- x |>
        dplyr::filter(!!sym(names(x_grps)) == lvl)
      x_f <- x_f[!sapply(x_f[[x_last]], is.null), ]

      x_inner <- x_f |>
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

      ards <- c(ards, list(tbl_stack(x_inner, .combine = TRUE)))
    }
    return(ards)
  }

  browser()
  outer_cols <- cards |>
    dplyr::filter(variable == h[1]) |>
    select(cards::all_ard_groups()) |>
    select(last_col(1):last_col()) |>
    names()

  cards_outer <- cards |>
    dplyr::filter(variable == h[1]) |>
    dplyr::select(-outer_cols)


  # create groups for each hierarchy level combination
  x <- cards_outer |>
    dplyr::group_by(dplyr::across(c(
      cards::all_ard_groups(types = "levels"),
      variable
    )))
  if (!is_empty(by)) {
    x <- x |> dplyr::ungroup(group1_level)
  }

  ards <- list()

  browser()
  # build the table body pieces with bridge functions and stack them -----------
  x_outer <- x |>
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

  browser()

  # combine hierarchy sub-tables
  x <- tbl_stack(res, x_outer, .combine = TRUE)

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
    return(x)
  }

  context <- context |>
    dplyr::select(-variable)

  # add 'hierarchy' element to table_styling
  x$table_styling[["hierarchy"]] <- context

  # browser()

  labels <- context |>
    unlist(use.names = FALSE) |>
    c()
  missing_labels <- if (ncol(context) > 1) labels |> head(-1) else labels
  n_labels <- length(labels)

  if (sum(x$table_body$row_type == "label") > 0) {
    # add label rows for each additional hierarchy level
    x$table_body <- x$table_body[rep(1, n_labels - 1), ] |>
      dplyr::mutate(
        var_label = missing_labels,
        label = missing_labels
      ) |>
      dplyr::bind_rows(x$table_body)

    # indent label rows for each hierarchy level
    for (i in seq(2, n_labels)) {
      x <- x |>
        modify_column_indent(
          columns = label,
          rows = row_type == "label" & var_label == labels[i],
          indent = (i - 1) * 4
        )
    }

    # indent non-label rows
    x <- x |>
      modify_column_indent(
        columns = label,
        rows = row_type != "label",
        indent = n_labels * 4
      )
  } else {
    # add label rows for each additional hierarchy level
    x$table_body <-
      tibble(
        variable = x$table_body$variable[1],
        row_type = "label",
        var_label = missing_labels,
        label = missing_labels
      ) |>
      dplyr::bind_rows(x$table_body)
  }

  x
}

ard_stack_hierarchical <- function(data,
                                   hierarchies,
                                   by = NULL,
                                   id = NULL,
                                   include,
                                   statistic,
                                   denominator,
                                   .overall = FALSE,
                                   .missing = FALSE,
                                   .attributes = FALSE,
                                   .total_n = FALSE,
                                   .shuffle = FALSE) {
  ard_lvls <- list()
  for (i in seq_along(hierarchies)) {
    ard_lvls <- c(
      ard_lvls,
      list(
        cards::ard_hierarchical(
          data = data,
          variables = hierarchies[1:i],
          by = all_of(by),
          statistic = statistic,
          denominator = denominator,
          id = all_of(id)
        )
      )
    )
  }

  ard_lvls |>
    dplyr::bind_rows() |>
    cards::tidy_ard_column_order()
}
