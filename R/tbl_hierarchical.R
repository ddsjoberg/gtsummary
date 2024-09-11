#' Create hierarchical table
#'
#' @examples
#' tbl_hierarchical(
#'   data = cards::ADAE |>
#'     dplyr::filter(
#'       AESOC %in% unique(ADAE$AESOC)[1:3],
#'       AETERM %in% unique(ADAE$AETERM)[1:3]
#'     ),
#'   hierarchies = c(AESOC, AETERM),
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
                             statistic = ifelse(
                               !missing(id),
                               list(all_categorical() ~ "{n} ({p})"),
                               list(all_categorical() ~ "{n}")
                             ),
                             digits = NULL,
                             overall_row = FALSE) {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(hierarchies)
  # check_string(statistic)
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
    cil::cli_abort(
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

  # TODO: Add digits argument processing

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())

  # calculate statistics -------------------------------------------------------
  # TODO: Update this with cards::ard_stack_hierarchical() when it's ready
  cards <-
    cards::ard_hierarchical(
      data = data,
      variables = all_of(hierarchies),
      by = all_of(by),
      denominator = denominator,
      id = all_of(id)
    )

  type <- assign_summary_type(data, include, value = NULL)
  # browser()
  # statistic <- list(all_categorical() ~ statistic)

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
  cards <- .add_gts_column_to_cards_summary(cards, include, by)

  # browser()
  # call bridge function here
  brdg_hierarchical(
    cards,
    hierarchies,
    type,
    by,
    id,
    include,
    statistic
  )
}


brdg_hierarchical <- function(cards,
                              hierarchies,
                              type,
                              by,
                              id,
                              include,
                              statistic,
                              missing = "no",
                              missing_stat = "{N_miss}",
                              missing_text = "Unknown") {
  # browser()
  set_cli_abort_call()

  browser()
  # build the table body pieces with bridge functions and stack them -----------
  res <- cards |>
    group_by(group2_level, variable, variable_level) |>
    group_map(
      function(.x, .y) {
        browser()
        .x <- .x |> as_card()

        brdg_summary(
          .x,
          .y$variable,
          type,
          statistic,
          by,
          missing,
          missing_stat,
          missing_test
        )
        table_body <- dplyr::left_join(
          dplyr::tibble(
            variable = .y$variable,
            var_type = type[.y$variable] |> unlist() |> unname()
          ),
          dplyr::bind_rows(
            pier_summary_continuous(
              cards = .x,
              variables = .get_variables_by_type(type, type = "continuous"),
              statistic = statistic
            ),
            pier_summary_continuous2(
              cards = .x,
              variables = .get_variables_by_type(type, type = "continuous2"),
              statistic = statistic
            ),
            pier_summary_categorical(
              cards = .x,
              variables = .get_variables_by_type(type, type = "categorical"),
              statistic = statistic
            ),
            pier_summary_dichotomous(
              cards = .x,
              variables = .get_variables_by_type(type, type = "dichotomous"),
              statistic = statistic
            ),
            pier_summary_missing_row(
              cards = .x,
              variables = hierarchies,
              missing = missing,
              missing_stat = missing_stat,
              missing_text = missing_text
            )
          ),
          by = "variable"
        )

        # construct default table_styling --------------------------------------------
        x <- .create_gtsummary_object(table_body)

        # add info to x$table_styling$header for dynamic headers ---------------------
        x <- .add_table_styling_stats(x, cards = cards, by = by)

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
              .construct_summary_footnote(cards, .y$variable, statistic, type)
          )

        x |>
          structure(class = "gtsummary") |>
          modify_column_unhide(columns = all_stat_cols())
      },
      .keep = TRUE
    )

  res
}
