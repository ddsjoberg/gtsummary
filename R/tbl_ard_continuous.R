#' Summarize continuous variable
#'
#' `r lifecycle::badge("experimental")`\cr
#' Summarize a continuous variable by one or more categorical variables
#'
#' @param variable (`string`)\cr
#'   A single variable name of the continuous variable being summarized.
#' @param by (`string`)\cr
#'   A single variable name of the stratifying variable.
#' @param include (`character`)\cr
#'   Character vector of the categorical variables to
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Specifies summary statistics to display for each variable.  The default is
#'   `everything() ~ "{median} ({p25}, {p75})"`.
#' @inheritParams tbl_ard_summary
#' @inheritParams tbl_continuous
#'
#' @return a gtsummary table of class `"tbl_ard_summary"`
#' @export
#'
#' @examples
#' library(cards)
#'
#' # Example 1 ----------------------------------
#' # the primary ARD with the results
#' ard_continuous(
#'   # the order variables are passed is important for the `by` variable.
#'   # 'trt' is the column stratifying variable and needs to be listed first.
#'   trial, by = c(trt, grade), variables = age
#' ) |>
#'   # adding OPTIONAL information about the summary variables
#'   bind_ard(
#'     # add univariate trt tabulation
#'     ard_categorical(trial, variables = trt),
#'     # add missing and attributes ARD
#'     ard_missing(trial, by = c(trt, grade), variables = age),
#'     ard_attributes(trial, variables = c(trt, grade, age))
#'   ) |>
#'   tbl_ard_continuous(by = "trt", variable = "age", include = "grade")
#'
#' # Example 2 ----------------------------------
#' # the primary ARD with the results
#' ard_continuous(trial, by = grade, variables = age) |>
#'   # adding OPTIONAL information about the summary variables
#'   bind_ard(
#'     # add missing and attributes ARD
#'     ard_missing(trial, by = grade, variables = age),
#'     ard_attributes(trial, variables = c(grade, age))
#'   ) |>
#'   tbl_ard_continuous(variable = "age", include = "grade")
tbl_ard_continuous <- function(cards, variable, include, by = NULL, label = NULL,
                               statistic = everything() ~ "{median} ({p25}, {p75})",
                               value = NULL) {
  set_cli_abort_call()
  check_not_missing(cards)
  check_not_missing(variable)
  check_not_missing(include)
  check_class(
    cards, "card",
    message = c("The {.arg {arg_name}} argument must be class {.cls {'card'}}, not {.obj_type_friendly {x}}.",
                i = "Some operations cause a {.cls {'card'}} data frame to lose its class; use {.fun cards::as_card} to restore it as needed.")
  )

  # define a data frame based on the context of `card` -------------------------
  data <- bootstrap_df_from_cards(cards)

  cards::process_selectors(data, variable = {{ variable }}, include = {{ include }}, by = {{ by }})
  include <- setdiff(include, c(by, variable)) # remove by variable from list vars included
  check_scalar(variable)
  check_scalar(
    by,
    allow_empty = TRUE,
    message = c("The {.arg {arg_name}} argument must be length {.val {1}} or empty.",
                i = "Use {.fun tbl_strata} for more than one {.arg by} variable."
    )
  )

  # process type and value arguments
  cards::process_formula_selectors(data[include], value = value)
  type <- rep_named(include, list("categorical")) |>
    utils::modifyList(rep_named(names(compact(value)), list("dichotomous")))

  # check the structure of the cards object ------------------------------------
  # check that the continuous variable appears somewhere in `cards$variable`
  if (!"variable" %in% names(cards) || !variable %in% cards$variable) {
    cli::cli_abort(
      "The continuous variable specified in argument {.arg variable} must
       appear in the column {.code cards$variable}.",
      call = get_cli_abort_call()
    )
  }
  if (!is_empty(by) && (!"group2" %in% names(cards) || !all(include %in% cards$group2))) {
    cli::cli_abort(
      "All variables specified in argument {.arg include} must
       appear in the column {.code cards$group2}.",
      call = get_cli_abort_call()
    )
  }
  if (is_empty(by) && (!"group1" %in% names(cards) || !all(include %in% cards$group1))) {
    cli::cli_abort(
      "All variables specified in argument {.arg include} must
       appear in the column {.code cards$group1}.",
      call = get_cli_abort_call()
    )
  }
  if (!is_empty(by) && (!"group1" %in% names(cards) || !by %in% cards$group1)) {
    cli::cli_abort(
      "The variables specified in argument {.arg by} must
       appear in the column {.code cards$group1}.",
      call = get_cli_abort_call()
    )
  }

  cards::check_list_elements(
    x = value,
    predicate = \(x) length(x) == 1L,
    error_msg =
      c("Error in argument {.arg {arg_name}} for variable {.val {variable}}.",
        "i" = "Elements values must be a scalar.")
  )

  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic = statistic,
    include_env = TRUE
  )
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[c(include, variable)]),
    label = label
  )

  # add the calling env to the statistics
  statistic <- .add_env_to_list_elements(statistic, env = caller_env())
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) is_string(x),
    error_msg = "Elements of the {.arg statistic} argument must be strings."
  )

  # save processed function inputs ---------------------------------------------
  tbl_ard_continuous_inputs <- as.list(environment())
  tbl_ard_continuous_inputs$data <- NULL
  tbl_ard_continuous_inputs$type <- NULL
  call <- match.call()

  # subsetting ARD based on passed value ---------------------------------------
  cards <- .subset_card_based_on_value(cards, value, by)

  # add/update attributes ------------------------------------------------------
  cards <-
    cards::bind_ard(
      cards::ard_attributes(data, variables = all_of(c(include, variable))),
      cards,
      cards::ard_attributes(data, variables = all_of(names(label)), label = label),
      .update = TRUE,
      .quiet = TRUE
    )

  # fill NULL stats with NA
  cards <- cards::replace_null_statistic(cards)

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <-
    cards::eval_capture_conditions(.add_gts_column_to_cards_continuous(cards, include, by)) |>
    cards::captured_condition_as_error(
      c("There was an assigning a {.pkg gtsummary} column name in the ARD.",
        "i" = "The error suggests a malformed ARD input in the {.arg cards} argument. See error message below:",
        "x" = "{condition}")
    )

  # prepare the base table via `brdg_continuous()` -----------------------------
  x <- brdg_continuous(cards, by = by, statistic = statistic, include = include,
                       variable = variable, type = type)

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(all_stat_cols() ~ "**{level}**")

  # prepend the footnote with information about the variable -------------------
  x$table_styling$footnote_header$footnote <-
    paste0(
      cards |>
        dplyr::filter(.data$context == "attributes", .data$variable == .env$variable, .data$stat_name == "label") |>
        dplyr::pull("stat") |>
        unlist(),
      ": ",
      x$table_styling$footnote_header$footnote
    )

  # add other information to the returned object
  x$inputs <- tbl_ard_continuous_inputs
  x$call_list <- list(tbl_ard_continuous = call)
  x$cards[["tbl_ard_continuous"]] <- cards

  x |>
    structure(class = c("tbl_ard_continuous", "gtsummary"))
}

.subset_card_based_on_value <- function(cards, value, by) {
  if (is_empty(value)) return(cards)

  if (is_empty(by)) {
    group <- "group1"
    group_level <- "group1_level"
  }
  else {
    group <- "group2"
    group_level <- "group2_level"
  }

  # every dichotomous variable, remove the levels we are not reporting
  for (varname in names(compact(value))) {
    # check value appears in ARD
    varlevels <- dplyr::filter(cards, .data[[group]] %in% .env$varname) |>
      dplyr::pull(all_of(group_level)) |>
      unlist() |>
      unique()
    if (!value[[varname]] %in% varlevels) {
      cli::cli_abort(
        c("There was an error in the {.arg value} argument for variable {.val {varname}}.",
          "The list value must be one of {.val {varlevels}}."),
        call = get_cli_abort_call()
      )
    }

    # removing other levels from the ARD for the variable
    cards <- cards |>
      dplyr::filter(!(.data[[group]] %in% .env$varname &
                        !map_lgl(.data[[group_level]], ~ !is.null(.x) && .x %in% .env$value[[.env$varname]])))
  }

  cards
}
