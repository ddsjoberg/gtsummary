#' Hierarchical Table
#'
#' @description `r lifecycle::badge('experimental')`\cr
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
#' A label for this overall row can be specified by passing an `'..ard_hierarchical_overall..'` element in `label`.
#' Similarly, the rounding for statistics in the overall row can be modified using the `digits` argument,
#' again referencing the `'..ard_hierarchical_overall..'` name.
#'
#' @return a gtsummary table of class `"tbl_hierarchical"` (for `tbl_hierarchical()`) or `"tbl_hierarchical_count"`
#'   (for `tbl_hierarchical_count()`).
#' @export
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
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
#'   digits = everything() ~ list(p = 1),
#'   overall_row = TRUE,
#'   label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
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
#'   overall_row = TRUE,
#'   label = list(..ard_hierarchical_overall.. = "Total Number of AEs")
#' )
tbl_hierarchical <- function(data,
                             variables,
                             id,
                             denominator,
                             by = NULL,
                             include = everything(),
                             statistic = everything() ~ "{n} ({p}%)",
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
    digits = {{ digits }},
    calling_fun = "tbl_hierarchical"
  )
}

#' @rdname tbl_hierarchical
#' @export
tbl_hierarchical_count <- function(data,
                                   variables,
                                   denominator = NULL,
                                   by = NULL,
                                   include = everything(),
                                   overall_row = FALSE,
                                   statistic = everything() ~ "{n}",
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
    statistic = statistic,
    overall_row = overall_row,
    label = label,
    digits = digits,
    calling_fun = "tbl_hierarchical_count"
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
                                      digits = NULL,
                                      calling_fun) {
  # process and check inputs ---------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_length(by, length = 1L, allow_empty = TRUE)
  check_logical(overall_row)
  if ("..ard_hierarchical_overall.." %in% variables) {
    cli::cli_abort("The {.arg variables} argument cannot include a column named {.val ..ard_hierarchical_overall..}.")
  }

  # evaluate tidyselect
  cards::process_selectors(data[variables], include = {{ include }})
  include <- union(include, dplyr::last(variables))
  anl_vars <- c(include, if (overall_row) "..ard_hierarchical_overall..")
  df_anl_vars <- data[include] |> dplyr::mutate(..ard_hierarchical_overall.. = data[[include[1]]]) # this df will be used for selecting below
  df_variables <- data[variables] |> dplyr::mutate(..ard_hierarchical_overall.. = data[[include[1]]]) # this df will be used for selecting below
  # check that 'include' is not empty, ie we must summarize at least one variable
  if (is_empty(include)) {
    cli::cli_abort(
      message = "Argument {.arg include} cannot be empty.",
      call = get_cli_abort_call()
    )
  }
  # add a default label to the overall variable
  if (overall_row) {
    attr(df_variables[["..ard_hierarchical_overall.."]], "label") <-
      switch(
        calling_fun,
        "tbl_hierarchical_count" = "Total number of events",
        "tbl_hierarchical" = "Number of patients with event"
      )
  }

  cards::process_formula_selectors(df_anl_vars, statistic = statistic, digits = digits) # statistic and label are defined for all vars that we summarize
  cards::process_formula_selectors(df_variables, label = label) # labels are only defined for the hierarchy variables

  # fill in unspecified variables
  cards::fill_formula_selectors(
    df_anl_vars,
    statistic = eval(formals(gtsummary::tbl_hierarchical)[["statistic"]])
  )
  cards::fill_formula_selectors(
    df_variables,
    label = lapply(names(df_variables), \(x) attr(df_variables[[x]], "label") %||% x) |> stats::setNames(names(df_variables))
  )

  # check that all statistics passed are strings
  if (calling_fun == "tbl_hierarchical") {
    cards::check_list_elements(
      x = statistic,
      predicate = \(x) is_string(x) && all(.extract_glue_elements(x) %in% c("n", "N", "p")),
      error_msg = "Values passed in the {.arg statistic} argument must be strings with glue elements containing one or more of {.val {c('n', 'N', 'p')}}."
    )
  }

  digits <-
    assign_summary_digits(
      data = data,
      statistic = statistic,
      type = rep_named(names(statistic), list("categorical")),
      digits = digits
    )
  digits <-
    case_switch(
      calling_fun == "tbl_hierarchical" ~
        lapply(digits, FUN = \(x) x[intersect(names(x), c("n", "N", "p"))]),
      calling_fun == "tbl_hierarchical_count" ~
        lapply(digits, FUN = \(x) x[intersect(names(x), "n")]),
    )

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())
  tbl_hierarchical_inputs$anl_vars <- NULL
  tbl_hierarchical_inputs$df_anl_vars <- NULL
  tbl_hierarchical_inputs$df_variables <- NULL
  tbl_hierarchical_inputs$calling_fun <- NULL
  if (calling_fun == "tbl_hierarchical_count") {
    tbl_hierarchical_inputs$id <- NULL
  }

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

  # apply digits ---------------------------------------------------------------
  cards <-
    cards |>
    dplyr::rows_update(
      imap(
        digits,
        ~ enframe(.x, "stat_name", "fmt_fn") |>
          dplyr::mutate(variable = .y)
      ) |>
        dplyr::bind_rows(),
      by = c("variable", "stat_name"),
      unmatched = "ignore"
    ) |>
    cards::apply_fmt_fn()

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_hierarchical(cards, variables, by)

  # call bridge function here
  brdg_hierarchical(
    cards = cards,
    variables = variables,
    by = by,
    include = include,
    statistic = statistic,
    overall_row = overall_row,
    count = is_empty(id),
    is_ordered = is.ordered(data[[dplyr::last(variables)]]),
    label = label
  ) |>
    append(
      list(
        cards = list(cards) |> stats::setNames(calling_fun),
        inputs = tbl_hierarchical_inputs
      )
    ) |>
    structure(class = c(calling_fun, "gtsummary"))
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
