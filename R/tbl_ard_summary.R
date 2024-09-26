#' ARD summary table
#'
#' `r lifecycle::badge("experimental")`\cr
#' The `tbl_ard_summary()` function tables descriptive statistics for
#' continuous, categorical, and dichotomous variables.
#' The functions accepts an ARD object.
#'
#' @param cards (`card`)\cr
#'   An ARD object of class `"card"` typically created with `cards::ard_*()` functions.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column from `data`. Summary statistics will be stratified by this variable.
#'   Default is `NULL`
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Used to specify the summary statistics for each variable.
#'   Each of the statistics must be present in `card` as no new statistics are calculated
#'   in this function.
#'   The default is
#'   `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#' @param missing,missing_text,missing_stat
#'   Arguments dictating how and if missing values are presented:
#'   - `missing`: must be one of `c("no", "ifany", "always")`
#'   - `missing_text`: string indicating text shown on missing row. Default is `"Unknown"`
#'   - `missing_stat`: statistic to show on missing row. Default is `"{N_miss}"`.
#'     Possible values are `N_miss`, `N_obs`, `N_nonmiss`, `p_miss`, `p_nonmiss`
#' @param type ([`formula-list-selector`][syntax])\cr
#'   Specifies the summary type. Accepted value are
#'   `c("continuous", "continuous2", "categorical", "dichotomous")`.
#'   Continuous summaries may be assigned `c("continuous", "continuous2")`, while
#'   categorical and dichotomous cannot be modified.
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to include in the summary table. Default is `everything()`
#' @param overall (scalar `logical`)\cr
#'   When `TRUE`, the `cards` input is parsed into two parts to run
#'   `tbl_ard_summary(cards_by) |> add_overall(cards_overall)`.
#'   Can only by used when `by` argument is specified.
#'   Default is `FALSE`.
#' @inheritParams tbl_summary
#'
#' @return a gtsummary table of class `"tbl_ard_summary"`
#' @export
#'
#' @examples
#' library(cards)
#'
#' ard_stack(
#'   data = ADSL,
#'   ard_categorical(variables = "AGEGR1"),
#'   ard_continuous(variables = "AGE"),
#'   .attributes = TRUE,
#'   .missing = TRUE,
#'   .total_n = TRUE
#' ) |>
#'   tbl_ard_summary()
#'
#' ard_stack(
#'   data = ADSL,
#'   .by = ARM,
#'   ard_categorical(variables = "AGEGR1"),
#'   ard_continuous(variables = "AGE"),
#'   .attributes = TRUE,
#'   .missing = TRUE,
#'   .total_n = TRUE
#' ) |>
#'   tbl_ard_summary(by = ARM)
#'
#' ard_stack(
#'   data = ADSL,
#'   .by = ARM,
#'   ard_categorical(variables = "AGEGR1"),
#'   ard_continuous(variables = "AGE"),
#'   .attributes = TRUE,
#'   .missing = TRUE,
#'   .total_n = TRUE,
#'   .overall = TRUE
#' ) |>
#'   tbl_ard_summary(by = ARM, overall = TRUE)
tbl_ard_summary <- function(cards,
                            by = NULL,
                            statistic = list(
                              all_continuous() ~ "{median} ({p25}, {p75})",
                              all_categorical() ~ "{n} ({p}%)"
                            ),
                            type = NULL,
                            label = NULL,
                            missing = c("no", "ifany", "always"),
                            missing_text = "Unknown",
                            missing_stat = "{N_miss}",
                            include = everything(),
                            overall = FALSE) {
  set_cli_abort_call()
  # data argument checks -------------------------------------------------------
  check_not_missing(cards)
  check_class(
    cards, "card",
    message = c("The {.arg {arg_name}} argument must be class {.cls {'card'}}, not {.obj_type_friendly {x}}.",
                i = "Some operations cause a {.cls {'card'}} data frame to lose its class; use {.fun cards::as_card} to restore it as needed.")
  )
  missing <- arg_match(missing)

  # define a data frame based on the context of `card` -------------------------
  data <- bootstrap_df_from_cards(cards)

  if (missing(missing_text)) {
    missing_text <-
      get_theme_element("tbl_summary-arg:missing_text", default = translate_string(missing_text)) # styler: off
  }
  check_string(missing_text)

  cards::process_selectors(data, include = {{ include }}, by = {{ by }})
  include <- setdiff(include, by) # remove by variable from list vars included
  check_scalar(by, allow_empty = TRUE)
  check_scalar_logical(overall)
  if (isTRUE(overall) && is_empty(by)) {
    cli::cli_inform(c("Cannot use {.code overall=TRUE} when {.arg by} argment not specified.",
                      "*" = "Setting {.code overall=FALSE}."))
    overall <- FALSE
  }

  # check structure of ARD input -----------------------------------------------
  if (is_empty(by) && !is_empty(names(dplyr::select(cards, cards::all_ard_groups())))) {
    cli::cli_abort(
      "The {.arg cards} object may not have group columns when the {.arg by} is empty.",
      call = get_cli_abort_call()
    )
  }
  else if (!is_empty(names(dplyr::select(cards, cards::all_ard_groups())) |> setdiff(c("group1", "group1_level")))) {
    cli::cli_abort(
      c("The {.arg cards} object may only contain a single stratifying variable.",
        i = "But contains {.val {names(dplyr::select(cards, cards::all_ard_groups())) |> setdiff(c('group1', 'group1_level'))}}."
      ),
      call = get_cli_abort_call()
    )
  }
  if (!is_empty(by) &&
      (!all(c("group1", "group1_level") %in% names(cards)) ||
       !all(stats::na.omit(cards$group1) %in% by))) {
    cli::cli_abort(
      "For {.code by = {.val {by}}}, columns {.val {c('group1', 'group1_level')}}
       must be present in {.arg cards} and {.val {'group1'}} must be equal to {.val {by}}.",
      call = get_cli_abort_call()
    )
  }

  # check the missing stats are available
  if (missing != "no") {
    include |>
      walk(
        \(.x) {
          if (dplyr::filter(cards, .data$variable %in% .env$.x, .data$context %in% "missing") |> nrow() == 0L) {
            cli::cli_abort(
              c("Argument {.code missing = {.val {missing}}} requires results from {.fun cards::ard_missing}, but they are missing for variable {.val {.x}}.",
                i = "Set {.code missing = {.val no}} to avoid printing missing counts.")
            )
          }
        }
      )
  }

  # temporary type so we can evaluate `statistic`, then we'll update it
  default_types <- dplyr::select(cards, "variable", "context") |>
    dplyr::distinct() |>
    dplyr::filter(
      .data$context %in% c("continuous", "categorical", "dichotomous"),
      .data$variable %in% .env$include
    ) |>
    deframe() |>
    as.list()
  # for non-standard ARDs, fill in the missing default types
  for (v in setdiff(include, names(default_types))) {
    if (!"variable_level" %in% names(cards) ||
        is_empty(compact(dplyr::filter(cards, .data$variable %in% .env$v)$variable_level))) {
      default_types[[v]] <- "continuous"
    }
    else default_types[[v]] <- "categorical" # styler: off
  }
  if (exists("v")) remove("v") # styler: off

  # process arguments ----------------------------------------------------------
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(default_types, "var_type"), data[include]),
    type = type,
    label = label
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(default_types, "var_type"), data[include]),
    type = default_types,
  )
  cards::check_list_elements(
    x = type,
    predicate = \(x) is.character(x) && length(x) == 1L && x %in% c("categorical", "dichotomous", "continuous", "continuous2"),
    error_msg = "Elements of the {.arg type} argumnet must be one of {.val {c('categorical', 'dichotomous', 'continuous', 'continuous2')}}."
  )
  # if the user passed `type` then check that the values are compatible with ARD summary types
  if (!missing(type)) {
    walk(
      include,
      function(variable) {
        if (isTRUE(default_types[[variable]] %in% "continuous") &&
            !type[[variable]] %in% c("continuous", "continuous2")) {
          cli::cli_abort(
            "Summary type for variable {.val {variable}} must be one of
             {.val {c('continuous', 'continuous2')}}, not {.val {type[[variable]]}}.",
            call = get_cli_abort_call()
          )
        } else if (isTRUE(default_types[[variable]] %in% c("categorical", "dichotomous")) &&
                   !identical(type[[variable]], default_types[[variable]])) {
          cli::cli_abort(
            "Summary type for variable {.val {variable}} must be
             {.val {default_types[[variable]]}}, not {.val {type[[variable]]}}.",
            call = get_cli_abort_call()
          )
        }
      }
    )
  }

  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic = statistic
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic = eval(formals(gtsummary::tbl_ard_summary)[["statistic"]]),
  )
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) is.character(x),
    error_msg = "The {.arg statistic} argument values must be class {.cls character} vector."
  )
  .check_stats_available(cards, statistic)

  walk(
    include,
    \(variable) {
      if (type[[variable]] %in% c("categorical", "dichotomous", "continuous") &&
          !is_string(statistic[[variable]])) {
        cli::cli_abort(
          "Variable {.val {variable}} is type {.arg {type[[variable]]}} and
           {.arg statistic} argument value must be a string of length one.",
          call = get_cli_abort_call()
        )
      }
    }
  )

  # add/update attributes ------------------------------------------------------
  cards <-
    cards::bind_ard(
      cards::ard_attributes(data, variables = all_of(include)),
      cards,
      cards::ard_attributes(data, variables = all_of(names(label)), label = label),
      .update = TRUE,
      .quiet = TRUE
    )

  # if `overall=TRUE`, parse cards into primary and overall parts --------------
  if (isTRUE(overall)) {
    cards_overall <- cards |>
      # remove grouped summary statistics
      dplyr::filter(is.na(.data$group1)) |>
      # remove `by` variable univariate tabulation
      dplyr::filter(!.data$variable %in% .env$by) |>
      dplyr::select(-(cards::all_missing_columns() & cards::all_ard_groups()))

    cards <- cards |>
      # remove univariate summary stats
      dplyr::filter(!(is.na(.data$group1) & .data$variable %in% .env$include) | .data$context %in% "attributes")
  }

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <-
    cards::eval_capture_conditions(.add_gts_column_to_cards_summary(cards, include, by)) |>
    cards::captured_condition_as_error(
      c("There was an assigning a {.pkg gtsummary} column name in the ARD.",
        "i" = "The error suggests a malformed ARD input in the {.arg cards} argument. See error message below:",
        "x" = "{condition}")
    )


  # save inputs
  tbl_ard_summary_inputs <- as.list(environment())[names(formals(tbl_ard_summary))]
  call <- match.call()

  # if overall=TRUE, then remove overall items from inputs object
  if (isTRUE(overall)) {
    tbl_ard_summary_inputs$overall <- FALSE
    tbl_ard_summary_inputs$cards_overall <- NULL
  }

  # fill NULL stats with NA
  cards <- cards::replace_null_statistic(cards)

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # construct initial tbl_ard_summary object --------------------------------------
  x <-
    brdg_summary(
      cards = cards,
      by = by,
      variables = include,
      statistic = statistic,
      type = type,
      missing = missing,
      missing_stat = missing_stat,
      missing_text = missing_text
    ) |>
    utils::modifyList(
      list(
        cards = list(tbl_ard_summary = cards),
        inputs = tbl_ard_summary_inputs
      )
    ) |>
    structure(class = c("tbl_ard_summary", "gtsummary"))

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(
      all_stat_cols() ~
        ifelse(
          is_empty(by),
          get_theme_element("tbl_summary-str:header-noby",
                            default = "**{level}**"),
          get_theme_element("tbl_summary-str:header-withby",
                            default = "**{level}**")
        )
    )

  # add call list to tbl_ard_summary table -------------------------------------
  x$call_list <- list(tbl_ard_summary = call)

  # if overall=TRUE, then run add_overall() ------------------------------------
  if (isTRUE(overall)) {
    x <- add_overall(x, cards = cards_overall)
  }

  # return tbl_ard_summary table -----------------------------------------------
  x
}
