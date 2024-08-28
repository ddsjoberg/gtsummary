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
#'
#' @return a gtsummary table of class `"tbl_ard_summary"`
#' @export
#'
#' @examples
#' library(cards)
#'
#' bind_ard(
#'   # the primary ARD with the results
#'   ard_continuous(
#'     trial,
#'     # the order variables are passed here is important.
#'     # 'trt' is the column stratifying variable and needs to be listed first.
#'     by = c(trt, grade),
#'     variables = age
#'   ),
#'   # add univariate trt tabulation
#'   ard_categorical(
#'     trial,
#'     variables = trt
#'   ),
#'   # add missing and attributes ARD
#'   ard_missing(
#'     trial,
#'     by = c(trt, grade),
#'     variables = age
#'   ),
#'   ard_attributes(
#'     trial,
#'     variables = c(trt, grade, age)
#'   )
#' ) |>
#'   tbl_ard_continuous(by = "trt", variable = "age", include = "grade")
#'
#' bind_ard(
#'   # the primary ARD with the results
#'   ard_continuous(trial, by = grade, variables = age),
#'   # add missing and attributes ARD
#'   ard_missing(trial, by = grade, variables = age),
#'   ard_attributes(trial, variables = c(grade, age))
#' ) |>
#'   tbl_ard_continuous(variable = "age", include = "grade")
tbl_ard_continuous <- function(cards, variable, include, by = NULL, label = NULL,
                               statistic = everything() ~ "{median} ({p25}, {p75})") {
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

  cards::process_formula_selectors(
    data[include],
    statistic = statistic,
    include_env = TRUE
  )
  cards::process_formula_selectors(
    data[include],
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
  call <- match.call()

  # add/update attributes ------------------------------------------------------
  cards <-
    cards::bind_ard(
      cards::ard_attributes(data, variables = all_of(include)),
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
  cards <- .add_gts_column_to_cards_continuous(cards, include, by)

  # prepare the base table via `brdg_continuous()` -----------------------------
  x <- brdg_continuous(cards, by = by, statistic = statistic, include = include, variable = variable)

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(all_stat_cols() ~ "**{level}**")

  # prepend the footnote with information about the variable -------------------
  x$table_styling$footnote$footnote <-
    paste0(
      cards |>
        dplyr::filter(.data$context == "attributes", .data$variable == .env$variable, .data$stat_name == "label") |>
        dplyr::pull("stat") |>
        unlist(),
      ": ",
      x$table_styling$footnote$footnote
    )

  # add other information to the returned object
  x$inputs <- tbl_ard_continuous_inputs
  x$call_list <- list(tbl_ard_continuous = call)
  x$cards[["tbl_ard_continuous"]] <- cards

  x |>
    structure(class = c("tbl_ard_continuous", "gtsummary"))
}
