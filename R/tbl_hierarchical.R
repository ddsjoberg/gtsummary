#' Create hierarchical table
#'
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables from `hierarchy` on whose levels (prior to nesting by the variable) summary statistics should be
#'   calculated for. Must include the last element of `hierarchy`.
#' @param summary_row ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables from `hierarchy` on whose levels (prior to nesting by the variable) a summary row should be displayed.
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
#'   denominator = cards::ADSL %>% mutate(TRTA = ARM),
#'   id = USUBJID
#' )
#'
#' @export
tbl_hierarchical <- function(data,
                             hierarchies,
                             by = dplyr::group_vars(data),
                             id = NULL,
                             label = NULL,
                             denominator = NULL,
                             include = everything(),
                             statistic = ifelse(!missing(id), "{n} ({p})", "{n}"),
                             digits = NULL,
                             summary_row = NULL) {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_not_missing(id)
  check_not_missing(denominator)
  check_not_missing(hierarchies)
  check_not_missing(include)
  check_data_frame(data)
  check_string(statistic)

  # evaluate tidyselect
  cards::process_selectors(data, hierarchies = {{ hierarchies }}, id = {{ id }}, by = {{ by }})

  # denominator must be a data frame, or integer
  if (!is.data.frame(denominator) && !is_integerish(denominator)) {
    cli::cli_abort(
      "The {.arg denominator} argument must be a {.cls data.frame} or an {.cls integer}, not {.obj_type_friendly {denominator}}.",
      call = get_cli_abort_call()
    )
  }

  # check the id argument is not empty
  if (is_empty(id)) {
    cli::cli_abort("Argument {.arg id} cannot be empty.", call = get_cli_abort_call())
  }

  # create table ---------------------------------------------------------------
  internal_tbl_hierarchical(
    data = data,
    hierarchies = hierarchies,
    by = by,
    id = id,
    label = label,
    denominator = denominator,
    include = include,
    statistic = statistic,
    digits = digits,
    summary_row = summary_row
  )
}

#' Create hierarchical table
#'
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables from `hierarchy` on whose levels (prior to nesting by the variable) summary statistics should be
#'   calculated for. Must include the last element of `hierarchy`.
#' @param summary_row ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables from `hierarchy` on whose levels (prior to nesting by the variable) a summary row should be displayed.
#'
#' @examples
#' data <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:5]
#'   )
#'
#' tbl_hierarchical_count(
#'   data = data,
#'   hierarchies = c(AESOC, AETERM, AESEV),
#'   by = TRTA,
#'   denominator = cards::ADSL %>% mutate(TRTA = ARM)
#' )
#'
#' @export
tbl_hierarchical_count <- function(data, # works
                                   hierarchies, # works
                                   by = dplyr::group_vars(data),
                                   label = NULL,
                                   denominator = NULL, # works
                                   include = everything(), # works
                                   digits = NULL,
                                   summary_row = NULL) { # works
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_not_missing(hierarchies)
  check_not_missing(include)
  check_data_frame(data)
  cards::process_selectors(data, hierarchies = {{ hierarchies }}, by = {{ by }})

  # denominator must be empty, a data frame, or integer
  if (!is_empty(denominator) && !is.data.frame(denominator) && !is_integerish(denominator)) {
    cli::cli_abort(
      "The {.arg denominator} argument must be empty, a {.cls data.frame}, or an {.cls integer}, not {.obj_type_friendly {denominator}}.",
      call = get_cli_abort_call()
    )
  }

  # create table ---------------------------------------------------------------
  internal_tbl_hierarchical(
    data = data,
    hierarchies = hierarchies,
    by = by,
    id = NULL,
    label = label,
    denominator = denominator,
    include = include,
    statistic = "{ n }",
    digits = digits,
    summary_row = summary_row
  )
}

internal_tbl_hierarchical <- function(data, # works
                                      hierarchies, # works
                                      by = dplyr::group_vars(data),
                                      id = NULL, # works
                                      label = NULL,
                                      denominator = NULL, # works
                                      include = everything(), # works
                                      statistic = NULL, # works
                                      digits = NULL,
                                      summary_row = NULL) {
  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_not_missing(hierarchies)
  check_not_missing(include)
  check_data_frame(data)

  # evaluate tidyselect
  cards::process_selectors(data[hierarchies], include = {{ include }}, summary_row = {{ summary_row }})

  # check that neither 'hierarchies' nor 'include' is empty
  if (is_empty(hierarchies) || is_empty(include)) {
    cli::cli_abort(
      message = "Arguments {.arg hierarchies} and {.arg include} cannot be empty.",
      call = get_cli_abort_call()
    )
  }

  # check that 'include' is a subset of 'hierarchies'
  if (!all(include %in% hierarchies)) {
    cli::cli_abort(
      message = c("The columns selected in {.arg include} must be nested within the columns in {.arg hierarchies}.")
    )
  }

  # check that 'summary_row' is a subset of 'hierarchies'
  if (!all(summary_row %in% hierarchies)) {
    cli::cli_abort(
      message = c("The columns selected in {.arg summary_row} must be nested within the columns in {.arg hierarchies}.")
    )
  }

  # check that the last hierarchy level is in either 'include' or 'summary_row'
  if (!tail(hierarchies, 1) %in% union(include, summary_row)) {
    cli::cli_abort(
      message = c(
        "The final variable of {.arg hierarchies} must be included in either {.arg include} or {.arg summary_row},",
        "otherwise this variable should be excluded from {.arg hierarchies} altogether.")
    )
  }

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())

  # get variable labels for each hierarchy level
  labels_hierarchy <- sapply(hierarchies, \(x) if (!is.null(attr(data[[x]], "label"))) attr(data[[x]], "label") else x)

  type <- assign_summary_type(data, union(by, hierarchies), value = NULL)

  if (!is_empty(statistic)) statistic <- as.formula(sprintf("everything() ~ \"%s\"", statistic))
  vars <- if (length(hierarchies) < 2) hierarchies else head(hierarchies, -1)

  cards <- .run_tbl_hierarchical_fun(
    data = data,
    hierarchies = hierarchies,
    by = by,
    denominator = denominator,
    id = id,
    vars = vars,
    summary_row = summary_row
  )
  cards_summary <- cards[[2]]
  cards <- cards[[1]]
  func <- if (!is_empty(id)) "tbl_hierarchical" else "tbl_hierarchical_count"

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[hierarchies]),
    statistic =
      case_switch(
        missing(statistic) ~ get_theme_element(paste0(func, "-arg:statistic"), default = statistic),
        .default = statistic
      ),
    include_env = TRUE
  )

  # add the calling env to the statistics
  statistic <- .add_env_to_list_elements(statistic, env = caller_env())

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[hierarchies]),
    label =
      case_switch(
        missing(label) ~ get_deprecated_theme_element(paste0(func, "-arg:label"), default = label),
        .default = label
      )
  )

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[hierarchies]),
    digits =
      case_switch(
        missing(digits) ~ get_theme_element(paste0(func, "-arg:digits"), default = digits),
        .default = digits
      )
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[hierarchies]),
    statistic = get_theme_element(
      paste0(func, "-arg:statistic"),
      default = eval(formals(asNamespace("gtsummary")[[func]])[["statistic"]])
    ),
    digits = get_theme_element(
      paste0(func, "-arg:digits"),
      default = eval(formals(asNamespace("gtsummary")[[func]])[["digits"]])
    )
  )

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <-
      scope_table_body(.list2tb(type, "var_type"), data[hierarchies]) |>
      assign_summary_digits(statistic, type, digits = digits)
  }

  # check inputs ---------------------------------------------------------------
  .check_haven_labelled(data[c(include, by)])
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = digits, value = NULL, type = type, sort = NULL
  )
  .check_statistic_type_agreement(statistic, type)

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_summary(cards, hierarchies, by, hierarchical = TRUE)

  if (is_empty(denominator)) denominator <- data

  # call bridge function here
  brdg_hierarchical(
    cards,
    hierarchies,
    type,
    by,
    include,
    statistic,
    cards_summary,
    labels_hierarchy
  ) |>
    append(
      list(
        cards = list(tbl_hierarchical = cards),
        inputs = tbl_hierarchical_inputs
      )
    ) |>
    structure(class = c(func, "gtsummary"))
}

# this function calculates either the counts or the rates of the events
.run_tbl_hierarchical_fun <- function(data, hierarchies, by, denominator, id, vars, summary_row) {
  # calculate statistics -------------------------------------------------------
  if (!is_empty(id)) {
    cards <- cards::ard_stack_hierarchical(
      data = data,
      variables = vars,
      by = c(by, setdiff(hierarchies, vars)),
      denominator = denominator,
      id = all_of(id),
      overall = TRUE,
      over_variables = TRUE
    )

    # calculate summary row statistics -------------------------------------------
    which_summarize <- c(hierarchies %in% summary_row, FALSE)
    cards_summary <- if (!is_empty(summary_row)) {
      cards::ard_stack_hierarchical(
        data = data,
        variables = head(hierarchies, -1),
        by = by,
        denominator = denominator,
        id = all_of(id)
      ) |>
        dplyr::filter(variable %in% c(by, hierarchies)[which_summarize]) |>
        .add_gts_column_to_cards_summary(head(hierarchies, -1), by, hierarchical = TRUE)
    } else {
      NULL
    }
  } else {
    # calculate statistics -------------------------------------------------------
    cards <- cards::ard_stack_hierarchical_count(
      data = data,
      variables = vars,
      by = c(by, setdiff(hierarchies, vars)),
      denominator = denominator,
      overall = TRUE,
      over_variables = TRUE
    )

    # calculate summary row statistics -------------------------------------------
    which_summarize <- c(hierarchies %in% summary_row, FALSE)
    cards_summary <- if (!is_empty(summary_row)) {
      cards::ard_stack_hierarchical_count(
        data = data,
        variables = head(hierarchies, -1),
        by = by,
        denominator = denominator
      ) |>
        dplyr::filter(variable %in% c(by, hierarchies)[which_summarize]) |>
        .add_gts_column_to_cards_summary(head(hierarchies, -1), by, hierarchical = TRUE)
    } else {
      NULL
    }
  }

  list(cards = cards, cards_summary = cards_summary)
}
