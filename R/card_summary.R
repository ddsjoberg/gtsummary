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
                         statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                                          all_categorical() ~ "{n} ({p}%)"),
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
  # - TODO: What other checks should we add?
  if (!is_empty(names(dplyr::select(cards, cards::all_ard_groups())) |> setdiff(c("group1", "group1_level")))) {
    cli::cli_abort(
      c("The {.arg cards} object may only contain a single stratifying variable.",
        i = "But contains {.val {names(dplyr::select(cards, cards::all_ard_groups())) |> setdiff(c('group1', 'group1_level'))}}."),
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
    dplyr::mutate(variable_level = map(.data$variable_level, ~.x %||% NA_real_)) |>
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "variable_level"
    ) |>
    dplyr::mutate(across(everything(), unlist))

  if ("group1" %in% names(cards)) by <- stats::na.omit(cards$group1)[1] |> unclass()
  else by <- character(0L)
  cards::process_selectors(data, include = {{ include }})
  include <- setdiff(include, by) # remove by variable from list vars included

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

  cards::process_formula_selectors(
    data = select_prep(.list2tb(type, "var_type"), data[include]),
    statistic = statistic
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    select_prep(.list2tb(type, "var_type"), data[include]),
    statistic = eval(formals(gtsummary::card_summary)[["statistic"]]),
  )

  # TODO: `type` should be an argument because there is no way to request a single line continuous2 summary.
  type <- imap(
    default_types,
    \(.x, .y) {
      if (.x == "continuous" && length(statistic[[.y]]) > 1L) return("continuous2")
      .x
    }
  )

  # save inputs
  card_summary_inputs <- as.list(environment())[names(formals(card_summary))]
  call <- match.call()

  # construct initial card_summary object --------------------------------------
  x <-
    list(
      cards = list(card_summary = cards),
      inputs = card_summary_inputs,
      call_list = list(card_summary = call)
    ) |>
    brdg_summary(
      calling_function = "card_summary",
      by = by,
      include = include,
      type = type
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

  # return tbl_summary table ---------------------------------------------------
  x
}
