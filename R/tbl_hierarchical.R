#' Hierarchical Table
#'
#' @description `r lifecycle::badge('experimental')`\cr
#' *This is an preview of this function. There will be changes in the coming releases, and changes will not undergo a formal deprecation cycle.*
#'
#' Use these functions to generate hierarchical tables.
#'
#' - `tbl_hierarchical()`: Calculates *rates* of events (e.g. adverse events)
#'   utilizing the `denominator` and `id` arguments to identify the rows in `data`
#'   to include in each rate calculation. If `variables` contains more than one
#'   variable and the last variable in `variables` is an ordered factor, then
#'   rates of events by highest level will be calculated.
#'
#' - `tbl_hierarchical_count()`: Calculates *counts* of events utilizing
#'   all rows for each tabulation.
#'
#' @param data (`data.frame`)\cr
#'   a data frame.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   character vector or tidy-selector of columns in data used to create a hierarchy. Hierarchy will be built with
#'   variables in the order given.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   a single column from `data`. Summary statistics will be stratified by this variable.
#'   Default is `NULL`.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   argument used to subset `data` to identify rows in `data` to calculate
#'   event rates in `tbl_hierarchical()`.
#' @param denominator (`data.frame`, `integer`)\cr
#'   used to define the denominator and enhance the output.
#'   The argument is required for `tbl_hierarchical()` and optional for `tbl_hierarchical_count()`.
#'   The `denominator` argument must be specified when `id` is used to calculate event rates.
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables from `hierarchy` for which summary statistics should be returned (on the variable label rows) Including
#'   the last element of `hierarchy` has no effect since each level has its own row for this variable.
#'   The default is `everything()`.
#' @param statistic (`string`)\cr
#'   used to specify the summary statistics to display for all variables in `tbl_hierarchical()`.
#'   The default is `"{n} ({p})"`.
#' @param overall_row (scalar `logical`)\cr
#'   whether an overall summary row should be included at the top of the table.
#'   The default is `FALSE`.
#' @param label ([`formula-list-selector`][syntax])\cr
#'   used to override default labels in hierarchical table, e.g. `list(AESOC = "System Organ Class")`.
#'   The default for each variable is the column label attribute, `attr(., 'label')`.
#'   If no label has been set, the column name is used.
#'
#' @section Overall Row:
#'
#' An overall row can be added to the table as the first row by specifying `overall_row = TRUE`. Assuming that each row
#' in `data` corresponds to one event record, this row will count the overall number of events recorded when used in
#' `tbl_hierarchical_count()`, or the overall number of patients recorded with any event when used in
#' `tbl_hierarchical()`.
#'
#' A label for this overall row can be specified by passing an `overall` element in `label`. If no `overall` label has
#' been set and `overall_row = TRUE`, `"Total number of patients with any event"` will be used by `tbl_hierarchical()`
#' and `"Total number of events"` will be used by `tbl_hierarchical_count()`.
#'
#' @return a gtsummary table of class `"tbl_hierarchical"` (for `tbl_hierarchical()`) or `"tbl_hierarchical_count"`
#'   (for `tbl_hierarchical_count()`).
#' @export
#'
#' @examples
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:5]
#'   )
#'
#' # Example 1 - Event Rates --------------------
#' tbl_hierarchical(
#'   data = ADAE_subset,
#'   variables = c(AESOC, AETERM),
#'   by = TRTA,
#'   denominator = cards::ADSL |> mutate(TRTA = ARM),
#'   id = USUBJID,
#'   overall_row = TRUE
#' )
#'
#' # Example 2 - Rates by Highest Severity ------
#' tbl_hierarchical(
#'   data = ADAE_subset |> mutate(AESEV = factor(AESEV, ordered = TRUE)),
#'   variables = c(AESOC, AESEV),
#'   by = TRTA,
#'   id = USUBJID,
#'   denominator = cards::ADSL |> mutate(TRTA = ARM),
#'   include = AESEV,
#'   label = list(AESEV = "Highest Severity")
#' )
#'
#' # Example 3 - Event Counts -------------------
#' tbl_hierarchical_count(
#'   data = ADAE_subset,
#'   variables = c(AESOC, AETERM, AESEV),
#'   by = TRTA,
#'   overall_row = TRUE
#' )
#'
tbl_hierarchical <- function(data,
                             variables,
                             by = NULL,
                             id = NULL,
                             denominator = NULL,
                             include = everything(),
                             statistic = "{n} ({p})",
                             overall_row = FALSE,
                             label = NULL) {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(id)
  check_not_missing(denominator)
  check_not_missing(variables)
  check_string(statistic)

  # evaluate tidyselect
  cards::process_selectors(data, variables = {{ variables }}, id = {{ id }}, by = {{ by }})

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
    variables = variables,
    by = by,
    id = id,
    denominator = denominator,
    include = {{ include }},
    statistic = statistic,
    overall_row = overall_row,
    label = label
  )
}

#' @rdname tbl_hierarchical
#' @export
tbl_hierarchical_count <- function(data,
                                   variables,
                                   by = NULL,
                                   denominator = NULL,
                                   include = everything(),
                                   overall_row = FALSE,
                                   label = NULL) {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(variables)

  # evaluate tidyselect
  cards::process_selectors(data, variables = {{ variables }}, by = {{ by }})

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
    variables = variables,
    by = by,
    id = NULL,
    denominator = denominator,
    include = {{ include }},
    statistic = "{n}",
    overall_row = overall_row,
    label = label
  )
}

internal_tbl_hierarchical <- function(data,
                                      variables,
                                      by = NULL,
                                      id = NULL,
                                      denominator = NULL,
                                      include = everything(),
                                      statistic = NULL,
                                      overall_row = FALSE,
                                      label = NULL) {
  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(variables)
  check_length(by, 1, allow_empty = TRUE)
  check_logical(overall_row)

  # evaluate tidyselect
  cards::process_selectors(data[variables], include = {{ include }})

  # check that 'variables' is not empty
  if (is_empty(variables)) {
    cli::cli_abort(
      message = "Argument {.arg variables} cannot be empty.",
      call = get_cli_abort_call()
    )
  }

  # check that 'include' is a subset of 'variables'
  if (!is_empty(include) && !all(include %in% variables)) {
    cli::cli_abort(
      message = c(
        "The columns selected in {.arg include} must be nested within the columns in {.arg variables}.",
        i = "Select among columns {.val {variables}}"
      )
    )
  }

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())

  # process arguments
  type <- assign_summary_type(data, c(variables, if (overall_row) by), value = NULL)
  func <- if (!is_empty(id)) "tbl_hierarchical" else "tbl_hierarchical_count"
  if (!is_empty(statistic)) {
    stat <- gsub("[\\{\\}]", "", regmatches(statistic, gregexpr("\\{.*?\\}", statistic))[[1]])
    stat <- stats::as.formula(sprintf("everything() ~ c('%s')", paste0(stat, collapse = "', '")))
    statistic <- stats::as.formula(sprintf("everything() ~ '%s'", statistic))
  } else {
    stat <- NULL
  }
  include <- union(include, dplyr::last(variables))

  # get ARDs -------------------------------------------------------------------
  cards <- .run_ard_stack_hierarchical_fun(
    data = data,
    variables = variables,
    by = by,
    id = id,
    denominator = denominator,
    include = include,
    statistic = NULL,
    overall_row = overall_row
  )

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic =
      case_switch(
        missing(statistic) ~ get_theme_element(paste0(func, "-arg:statistic"), default = statistic),
        .default = statistic
      ),
    include_env = TRUE
  )
  cards::process_formula_selectors(
    scope_table_body(
      .list2tb(type, "var_type"),
      data[variables] |>
        mutate(overall = if (overall_row) NA else NULL)
    ),
    label =
      case_switch(
        missing(label) ~ get_theme_element(paste0(func, "-arg:label"), default = label),
        .default = label
      )
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic = get_theme_element(
      paste0(func, "-arg:statistic"), default = eval(formals(asNamespace("gtsummary")[[func]])[["statistic"]])
    )
  )

  # check inputs ---------------------------------------------------------------
  .check_haven_labelled(data[c(include, by)])
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = NULL, value = NULL, type = type, sort = NULL
  )
  .check_statistic_type_agreement(statistic, type)

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_summary(cards, variables, by, hierarchical = TRUE)

  # fill in missing labels -----------------------------------------------------
  default_label <- sapply(
    variables,
    \(x) if (!is_empty(attr(data[[x]], "label"))) attr(data[[x]], "label") else x
  ) |>
    as.list()
  label <- c(
    label, default_label[setdiff(names(default_label), names(label))]
  )[c(variables, if ("overall" %in% names(label)) "overall")]

  # define overall row formatting
  if (overall_row) {
    statistic[["overall"]] <- statistic[1]
    type[["overall"]] <- "categorical"
    if (!"overall" %in% names(label)) {
      label[["overall"]] <- ifelse(
        func == "tbl_hierarchical_count",
        "Total number of events",
        "Total number of patients with any event"
      )
    }
  }

  # call bridge function here
  brdg_hierarchical(
    cards,
    variables,
    by,
    include,
    statistic,
    type,
    overall_row,
    count = is_empty(id),
    is_ordered = is.ordered(data[[dplyr::last(variables)]]),
    label
  ) |>
    append(
      list(
        cards = list(cards) |> stats::setNames(func),
        inputs = tbl_hierarchical_inputs
      )
    ) |>
    structure(class = c("tbl_hierarchical", "gtsummary"))
}

# this function calculates either the counts or the rates of the events
.run_ard_stack_hierarchical_fun <- function(data, variables, by, id, denominator, include, statistic, overall_row) {
  if (!is_empty(id)) {
    # for ordered factor variable, move last hierarchy level to by
    # to get rates by highest level
    cards_ord <- list()
    if (!is.ordered(data[[dplyr::last(variables)]]) || length(variables) == 1) {
      # only one hierarchy variable - ignore ordering
      data[[dplyr::last(variables)]] <- factor(data[[dplyr::last(variables)]], ordered = FALSE)
    } else {
      cards_ord <- cards::ard_stack_hierarchical(
        data = data,
        variables = all_of(utils::head(variables, -1)),
        by = all_of(c(by, dplyr::last(variables))),
        id = all_of(id),
        denominator = denominator,
        include = all_of(dplyr::nth(variables, -2)),
        statistic = statistic,
        total_n = (is_empty(by) && length(include) == 1)
      )

      # update structure to match results for non-ordered factor variables
      which_var <- which(names(cards_ord) == "variable")
      which_h <- which(names(cards_ord) == paste0("group", length(by) + 1))
      names(cards_ord) <- names(cards_ord)[
        c(0:(which_h - 1), which_var + 0:1, which_h:(which_var - 1), (which_var + 2):length(names(cards_ord)))
      ]

      # if no other statistics to calculate, format N data and return as is
      # otherwise, bind to results for the remaining include variables
      variables <- utils::head(variables, -1)
      include <- intersect(include, variables)
      if (is_empty(include)) {
        cards_ord[cards_ord[[which_var]] %in% by, which_h + 0:1] <-
          cards_ord[cards_ord[[which_var]] %in% by, which_var + 0:1]
        return(cards_ord)
      } else if (!is_empty(by)) {
        cards_ord <- cards_ord |>
          dplyr::filter(.data$group1 == by[1] | .data$context == "total_n")
      }
    }

    cards <- cards::ard_stack_hierarchical(
      data = data,
      variables = all_of(variables),
      by = any_of(by),
      id = all_of(id),
      denominator = denominator,
      include = all_of(include),
      statistic = statistic,
      over_variables = overall_row,
      total_n = is_empty(by)
    )

    cards::bind_ard(cards, cards_ord)
  } else {
    cards::ard_stack_hierarchical_count(
      data = data,
      variables = all_of(variables),
      by = any_of(by),
      denominator = denominator,
      include = all_of(include),
      over_variables = overall_row,
      total_n = is_empty(by) && !is_empty(denominator)
    )
  }
}
