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
#'   id = USUBJID,
#'   lvl_groups = AESEV ~ c("- Any Intensity -" = c("MILD", "MODERATE", "SEVERE"))
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
                             overall_row = FALSE,
                             lvl_groups = NULL) {
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
  # TODO: figure out include argument
  # if (!setequal(include, hierarchies[seq_len(length(include))])) {
  #   cli::cli_abort(
  #     message = c("The columns selected in {.arg include} must be nested within the columns in {.arg hierarchies}",
  #                 "i" = "For example, when {.code hierarchies = c(SOC, AETERM)}, {.arg include} can be {.code AETERM} but not {.code SOC}.")
  #   )
  # }

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())

  # get variable labels for each hierarchy level
  labels_hierarchy <- sapply(hierarchies, \(x) if (!is.null(attr(data[[x]], "label"))) attr(data[[x]], "label") else x)

  type <- assign_summary_type(data, include, value = NULL)

  statistic <- as.formula(sprintf("all_categorical() ~ \"%s\"", statistic))

  # browser()
  # # evaluate the remaining list-formula arguments ------------------------------
  # # processed arguments are saved into this env
  # cards::process_formula_selectors(
  #   data = scope_table_body(.list2tb(type, "var_type"), data[include]),
  #   lvl_groups =
  #     case_switch(
  #       missing(lvl_groups) ~ get_theme_element("tbl_hierarchical-arg:lvl_groups", default = lvl_groups),
  #       .default = lvl_groups
  #     ),
  #   include_env = TRUE
  # )
  #
  # # add the calling env to the statistics
  # lvl_groups <- .add_env_to_list_elements(lvl_groups, env = caller_env())

  # calculate statistics -------------------------------------------------------
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
        missing(statistic) ~ get_theme_element("tbl_hierarchical-arg:statistic", default = statistic),
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
        missing(label) ~ get_deprecated_theme_element("tbl_hierarchical-arg:label", default = label),
        .default = label
      )
  )

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    digits =
      case_switch(
        missing(digits) ~ get_theme_element("tbl_hierarchical-arg:digits", default = digits),
        .default = digits
      )
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic =
      get_theme_element("tbl_hierarchical-arg:statistic", default = eval(formals(gtsummary::tbl_hierarchical)[["statistic"]])),
    digits =
      get_theme_element("tbl_hierarchical-arg:digits", default = eval(formals(gtsummary::tbl_hierarchical)[["digits"]]))
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

  if (is_empty(denominator)) denominator <- data

  # add total N rows to 'cards' - not needed if incorporated into ard_stack_hierarchical()
  cards <- cards::bind_ard(
    cards,
    cards::ard_total_n(data = denominator) |>
      dplyr::mutate(gts_column = NA),
    case_switch(
      !is_empty(by) ~
        cards::ard_categorical(
          denominator,
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
