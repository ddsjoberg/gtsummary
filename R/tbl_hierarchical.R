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
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   used to specify the summary statistics to display for all variables in `tbl_hierarchical()`.
#'   The default is `everything() ~ "{n} ({p})"`.
#' @param overall_row (scalar `logical`)\cr
#'   whether an overall summary row should be included at the top of the table.
#'   The default is `FALSE`.
#' @param label ([`formula-list-selector`][syntax])\cr
#'   used to override default labels in hierarchical table, e.g. `list(AESOC = "System Organ Class")`.
#'   The default for each variable is the column label attribute, `attr(., 'label')`.
#'   If no label has been set, the column name is used.
#' @param digits ([`formula-list-selector`][syntax])\cr
#'  Specifies how summary statistics are rounded. Values may be either integer(s) or function(s). If not specified,
#'  default formatting is assigned via `label_style_number()` for statistics `n` and `N`, and
#'  `label_style_percent(digits=1)` for statistic `p`.
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
#'   overall_row = TRUE,
#'   digits = everything() ~ list(p = 2)
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
                             statistic = ~"{n} ({p}%)",
                             overall_row = FALSE,
                             label = NULL,
                             digits = NULL) {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(id)
  check_not_missing(denominator)
  check_not_missing(variables)

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
    statistic = {{ statistic }},
    overall_row = overall_row,
    label = label,
    digits = {{ digits }}
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
                                   label = NULL,
                                   digits = NULL) {
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
    statistic = ~"{n}",
    overall_row = overall_row,
    label = label,
    digits = digits
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
                                      label = NULL,
                                      digits = NULL) {
  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(variables)
  check_length(by, 1, allow_empty = TRUE)
  check_logical(overall_row)

  # evaluate tidyselect
  cards::process_selectors(data[variables], include = {{ include }})
  anl_vars <- c(union(include, dplyr::last(variables)), if (overall_row) by)
  cards::process_formula_selectors(data[anl_vars], statistic = statistic, digits = digits)

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

  # check that all of data[anl_vars] are categorical type variables
  type <- assign_summary_type(data, anl_vars, value = NULL)
  if (!all(type == "categorical")) {
    cli::cli_abort(
      "The columns selected in {.arg variables} and {.arg by} must all be {.cls character} or {.cls factor}.",
      call = get_cli_abort_call()
    )
  }

  # check that all statistics passed are strings
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) is_string(x),
    error_msg = "Values passed in the {.arg statistic} argument must be strings."
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    data[anl_vars],
    statistic = eval(formals(gtsummary::tbl_hierarchical)[["statistic"]])
  )

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <- .assign_hierarchical_digits(data, statistic, digits)
  }

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())

  # process arguments
  func <- if (!is_empty(id)) "tbl_hierarchical" else "tbl_hierarchical_count"
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
    data = scope_table_body(.list2tb(type, "var_type"), data[anl_vars]),
    statistic =
      case_switch(
        missing(statistic) ~ get_theme_element(paste0(func, "-arg:statistic"), default = statistic),
        .default = statistic
      ),
    include_env = TRUE
  )
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[anl_vars]),
    digits =
      case_switch(
        missing(digits) ~ get_theme_element(paste0(func, "-arg:digits"), default = digits),
        .default = digits
      )
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
    scope_table_body(.list2tb(type, "var_type"), data[anl_vars]),
    statistic = get_theme_element(
      paste0(func, "-arg:statistic"), default = eval(formals(asNamespace("gtsummary")[[func]])[["statistic"]])
    )
  )

  # apply digits ---------------------------------------------------------------
  names(digits)[names(digits) == by] <- "..ard_hierarchical_overall.."
  for (v in names(digits)) {
    for (stat in lapply(statistic, function(x) .extract_glue_elements(x) |> unlist())[[v]]) {
      cards <- cards |>
        cards::update_ard_fmt_fn(variables = all_of(v), stat_names = stat, fmt_fn = digits[[v]][[stat]])
    }
  }
  cards <- cards |> cards::apply_fmt_fn()

  # check inputs ---------------------------------------------------------------
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = digits, value = NULL, type = type, sort = NULL
  )

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_hierarchical(cards, variables, by)

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

.add_gts_column_to_cards_hierarchical <- function(cards, variables, by) {
  # adding the name of the column the stats will populate
  if (is_empty(by)) {
    cards$gts_column <-
      ifelse(
        !cards$context %in% "attributes" & !cards$variable %in% "..ard_total_n..",
        "stat_0",
        NA_character_
      )
  } else {
    cards <- cards |>
      dplyr::group_by(.data$group1_level) |>
      dplyr::mutate(gts_column = paste0("stat_", dplyr::cur_group_id()))

    # process overall row
    cards[cards$variable %in% by, ] <- cards[cards$variable %in% by, ] |>
      dplyr::group_by(.data$variable_level) |>
      dplyr::mutate(gts_column = paste0("stat_", dplyr::cur_group_id()))
  }

  cards |>
    dplyr::ungroup() |>
    cards::as_card()
}

.assign_hierarchical_digits <- function(data, statistic, digits = NULL) {
  set_cli_abort_call()

  lst_cat_summary_fns <- c(
    c("n", "N") |> rep_named(list(label_style_number())),
    c("p") |>
      rep_named(list(get_theme_element("tbl_summary-fn:percent_fun", default = label_style_percent(digits = 1))))
  )

  # extract the statistics
  statistic <- lapply(statistic, function(x) .extract_glue_elements(x) |> unlist())

  lapply(
    names(statistic),
    function(variable) {
      # if user passed digits AND they've specified every statistic, use the passed value
      # otherwise, we need to calculate the defaults, and later we can update with the pieces the user passed
      if (!is.null(digits[[variable]])) {
        # if a scalar or vector passed, convert it to a list
        if (!is.list(digits[[variable]]) && is_vector(digits[[variable]])) {
          digits[[variable]] <- as.list(digits[[variable]])
        }

        # if user-passed value is not named, repeat the passed value to the length of 'statistic'
        if (!is_named(digits[[variable]])) {
          if (!is_function(digits[[variable]])) digits[[variable]] <- rep_named(statistic[[variable]], digits[[variable]])
          else digits[[variable]] <- rep_named(statistic[[variable]], digits[variable])
        }

        # convert integers to a proper function
        digits[[variable]] <- .convert_integer_to_fmt_fn(digits[[variable]])

        # check value is a function
        if (!is_list(digits[[variable]]) || some(digits[[variable]], \(.x) !is_function(.x))) {
          cli::cli_abort(
            c("Error in {.arg digits} argument for variable {.val {variable}},",
              i = "Passed values must be either a {.cls function} or {.cls integer}."),
            call = get_cli_abort_call()
          )
        }

        # if the passed value fully specifies the formatting for each 'statistic',
        # then return it. Otherwise, the remaining stat will be filled below
        if (setequal(statistic[[variable]], names(digits[[variable]]))) {
          return(lst_cat_summary_fns |> utils::modifyList(digits[[variable]]))
        }
      }

      return(lst_cat_summary_fns |> utils::modifyList(digits[[variable]] %||% list()))
    }
  ) |>
    stats::setNames(names(statistic))
}
