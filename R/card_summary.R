#' ARD summary table
#'
#' The `card_summary()` function tables descriptive statistics for
#' continuous, categorical, and dichotomous variables.
#' The functions accepts an ARD object.
#'
#' @param cards (`card`)\cr
#'   An ARD object of class `"card"` typically created with `cards::ard_*()` functions.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Used to specify the summary statistics for each variable.
#'   Each of the statistics must be present in `card` as no new statistics are calculated
#'   in this function.
#'   The default is
#'   `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#' @param missing,missing_text,missing_stat
#'   Arguments dictating how and if missing values are presented:
#'   - `missing`: must be one of `c("ifany", "no", "always")`
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
#'
#' @return a gtsummary table of class `"card_summary"`
#' @export
#'
#' @examples
#' library(cards)
#'
#' ard_stack(
#'   data = ADSL,
#'   by = NULL,
#'   ard_categorical(variables = "AGEGR1"),
#'   ard_continuous(variables = "AGE"),
#'   .attributes = TRUE,
#'   .missing = TRUE
#' ) |>
#'   card_summary()
#'
#' ard_stack(
#'   data = ADSL,
#'   by = ARM,
#'   ard_categorical(variables = "AGEGR1"),
#'   ard_continuous(variables = "AGE"),
#'   .attributes = TRUE,
#'   .missing = TRUE
#' ) |>
#'   card_summary()
card_summary <- function(cards,
                         statistic = list(
                           all_continuous() ~ "{median} ({p25}, {p75})",
                           all_categorical() ~ "{n} ({p}%)"
                         ),
                         type = NULL,
                         missing = c("ifany", "no", "always"),
                         missing_text = "Unknown",
                         missing_stat = "{N_miss}",
                         include = everything()) {
  set_cli_abort_call()
  # data argument checks -------------------------------------------------------
  check_not_missing(cards)
  check_class(cards, "card")
  missing <- arg_match(missing)

  # check structure of ARD input -----------------------------------------------
  # TODO: What other checks should we add?
  if (!is_empty(names(dplyr::select(cards, cards::all_ard_groups())) |> setdiff(c("group1", "group1_level")))) {
    cli::cli_abort(
      c("The {.arg cards} object may only contain a single stratifying variable.",
        i = "But contains {.val {names(dplyr::select(cards, cards::all_ard_groups())) |> setdiff(c('group1', 'group1_level'))}}."
      ),
      call = get_cli_abort_call()
    )
  }

  # define a data frame based on the context of `card` -------------------------
  data <- cards |>
    dplyr::select(cards::all_ard_variables()) |>
    dplyr::bind_rows(
      dplyr::select(cards, cards::all_ard_groups()) |>
        dplyr::rename(variable = any_of("group1"), variable_level = any_of("group1_level"))
    ) |>
    dplyr::filter(!is.na(.data$variable)) |>
    dplyr::slice(.by = "variable", 1L) |>
    dplyr::mutate(variable_level = map(.data$variable_level, ~ .x %||% NA_real_)) |>
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "variable_level"
    ) |>
    dplyr::mutate(across(everything(), unlist))

  if ("group1" %in% names(cards)) {
    by <- stats::na.omit(cards$group1)[1] |> unclass()
  } else {
    by <- character(0L)
  }
  cards::process_selectors(data, include = {{ include }})
  include <- setdiff(include, by) # remove by variable from list vars included

  # check that each included variable has 'missing' and 'attributes' ARDs (this can probably be relaxed later)
  missing_or_attributes_ard <-
    imap(
      include,
      ~ dplyr::filter(cards, .data$variable %in% .env$.x, .data$context %in% c("missing", "attributes")) |>
        dplyr::select("variable", "context") |>
        dplyr::distinct() |>
        nrow() %>%
        {!identical(., 2L)} # styler: off
    ) |>
    set_names(include) |>
    unlist()
  if (any(missing_or_attributes_ard)) {
    cli::cli_abort(
      c("{.val {names(missing_or_attributes_ard)[missing_or_attributes_ard]}}
          {?does/do} not have associated {.field missing} or {.field attributes} ARD results.",
        i = "Use {.fun cards::ard_missing}, {.fun cards::ard_attributes}, or
             {.code cards::ard_stack(.missing=TRUE, .attributes=TRUE)} to calculate needed results."
      ),
      call = get_cli_abort_call()
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

  # process arguments ----------------------------------------------------------
  cards::process_formula_selectors(
    data = select_prep(.list2tb(default_types, "var_type"), data[include]),
    type = type
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    select_prep(.list2tb(default_types, "var_type"), data[include]),
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
        if (default_types[[variable]] %in% "continuous" &&
          !type[[variable]] %in% c("continuous", "continuous2")) {
          cli::cli_abort(
            "Summary type for variable {.val {variable}} must be one of
             {.val {c('continuous', 'continuous2')}}, not {.val {type[[variable]]}}.",
            call = get_cli_abort_call()
          )
        } else if (default_types[[variable]] %in% c("categorical", "dichotomous") &&
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
    data = select_prep(.list2tb(type, "var_type"), data[include]),
    statistic = statistic
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    select_prep(.list2tb(type, "var_type"), data[include]),
    statistic = eval(formals(gtsummary::card_summary)[["statistic"]]),
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
  # save inputs
  card_summary_inputs <- as.list(environment())[names(formals(card_summary))]
  call <- match.call()

  # construct initial card_summary object --------------------------------------
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
    append(
      list(
        cards = list(card_summary = cards),
        inputs = card_summary_inputs,
        call_list = list(card_summary = call)
      )
    ) |>
    structure(class = c("card_summary", "gtsummary"))

  # adding styling -------------------------------------------------------------
  x <- x |>
    # add header to label column and add default indentation
    modify_table_styling(
      columns = "label",
      label = "**Characteristic**",
      rows = .data$row_type %in% c("level", "missing"),
      indentation = 4L
    ) |>
    # adding the statistic footnote
    modify_table_styling(
      columns = all_stat_cols(),
      footnote =
        .construct_summary_footnote(x$cards[["card_summary"]], include, statistic, type)
    ) |>
    # updating the headers for the stats columns
    modify_header(
      all_stat_cols() ~ ifelse(is_empty(by), "**N = {N}**", "**{level}**  \nN = {n}")
    )

  # return card_summary table --------------------------------------------------
  x
}
