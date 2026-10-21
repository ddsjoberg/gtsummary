#' ARD Hierarchical Table
#'
#' @description `r lifecycle::badge('experimental')`\cr
#' *This is a preview of this function. There will be changes in the coming releases, and changes will not undergo a formal deprecation cycle.*
#'
#' Constructs tables from nested or hierarchical data  structures (e.g. adverse events).
#'
#' @inheritParams tbl_hierarchical
#' @inheritParams tbl_ard_summary
#' @param label ([`formula-list-selector`][syntax])\cr
#'   used to override default labels in hierarchical table, e.g. `list(AESOC = "System Organ Class")`.
#'   The default for each variable is the label saved in the ARD (e.g. when created with
#'   `cards::ard_stack_hierarchical(attributes = TRUE)`). If no label is present, the column name is used.
#'
#' @section Overall Row:
#'
#' An overall row can be added to the table as the first row by specifying `overall_row = TRUE`.
#' The `cards` object must contain the overall results, which are calculated by
#' `cards::ard_stack_hierarchical(over_variables = TRUE)` and `cards::ard_stack_hierarchical_count(over_variables = TRUE)`.
#'
#' A label for this overall row can be specified by passing an `'..ard_hierarchical_overall..'` element in `label`.
#'
#' @return a gtsummary table of class `"tbl_ard_hierarchical"`
#' @export
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:5]
#'   )
#'
#' # Example 1: Event Rates  --------------------
#' # First, build the ARD
#' ard <-
#'   cards::ard_stack_hierarchical(
#'     data = ADAE_subset,
#'     variables = c(AESOC, AETERM),
#'     by = TRTA,
#'     denominator = cards::ADSL,
#'     id = USUBJID,
#'     over_variables = TRUE,
#'     attributes = TRUE
#'   )
#'
#' # Second, build table from the ARD
#' tbl_ard_hierarchical(
#'   cards = ard,
#'   variables = c(AESOC, AETERM),
#'   by = TRTA,
#'   overall_row = TRUE,
#'   label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
#' )
#'
#' # Example 2: Event Counts  -------------------
#' ard <-
#'   cards::ard_stack_hierarchical_count(
#'     data = ADAE_subset,
#'     variables = c(AESOC, AETERM),
#'     by = TRTA,
#'     denominator = cards::ADSL
#'   )
#'
#' tbl_ard_hierarchical(
#'   cards = ard,
#'   variables = c(AESOC, AETERM),
#'   by = TRTA,
#'   statistic = ~"{n}"
#' )
tbl_ard_hierarchical <- function(cards,
                                 variables,
                                 by = NULL,
                                 include = everything(),
                                 statistic = ~"{n} ({p}%)",
                                 overall_row = FALSE,
                                 label = NULL) {
  set_cli_abort_call()

  # data argument checks -------------------------------------------------------
  check_not_missing(cards)
  check_class(
    cards, "card",
    message = c("The {.arg {arg_name}} argument must be class {.cls {'card'}}, not {.obj_type_friendly {x}}.",
                i = "Some operations cause a {.cls {'card'}} data frame to lose its class; use {.fun cards::as_card} to restore it as needed.")
  )
  check_not_missing(variables)
  check_scalar_logical(overall_row)

  # define a data frame based on the context of `card` -------------------------
  data <- bootstrap_df_from_cards(cards)

  cards::process_selectors(data, variables = {{ variables }}, by = {{ by }})
  cards::process_selectors(data[variables], include = {{ include }})
  check_length(by, length = 1L, allow_empty = TRUE)
  if ("..ard_hierarchical_overall.." %in% variables) {
    cli::cli_abort(
      "The {.arg variables} argument cannot include a column named {.val ..ard_hierarchical_overall..}.",
      call = get_cli_abort_call()
    )
  }
  if (is_empty(include)) {
    cli::cli_abort("Argument {.arg include} cannot be empty.", call = get_cli_abort_call())
  }

  # check structure of ARD input -----------------------------------------------
  .check_ard_hierarchical_structure(cards, variables, by, include, overall_row)
  summary_vars <- c(variables, if (overall_row) "..ard_hierarchical_overall..")

  cards::process_formula_selectors(data[intersect(summary_vars, unique(cards$variable))], statistic = statistic)

  # check that all statistics passed are strings
  cards::check_list_elements(
    x = statistic,
    predicate = \(x) is_string(x),
    error_msg = "Values passed in the {.arg statistic} argument must be strings."
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    data[intersect(summary_vars, unique(cards$variable))],
    statistic = eval(formals(gtsummary::tbl_ard_hierarchical)[["statistic"]])
  )
  .check_stats_available(cards, statistic)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_hierarchical(cards, variables, by)

  # save arguments
  tbl_ard_hierarchical_inputs <- as.list(environment())
  tbl_ard_hierarchical_inputs[["data"]] <- NULL
  tbl_ard_hierarchical_inputs[["summary_vars"]] <- NULL

  # apply formatting fns -------------------------------------------------------
  cards <- cards |> cards::apply_fmt_fun()

  # fill in missing labels -----------------------------------------------------
  # defaults are the label attributes saved in the ARD, otherwise the column name
  df_attr_label <- cards |>
    dplyr::filter(.data$context %in% "attributes", .data$stat_name %in% "label")
  default_label <- names(data) |> as.list() |> stats::setNames(names(data))
  default_label[intersect(df_attr_label$variable, names(data))] <-
    df_attr_label$stat[match(intersect(df_attr_label$variable, names(data)), df_attr_label$variable)] |>
    lapply(as.character)
  if (overall_row) {
    default_label[["..ard_hierarchical_overall.."]] <-
      ifelse(inherits(cards, "ard_stack_hierarchical_count"), "Total number of events", "Number of patients with event")
  }
  cards::process_formula_selectors(data[summary_vars], label = label)
  cards::fill_formula_selectors(data[summary_vars], label = default_label[summary_vars])
  label <- label[summary_vars]

  # add hierarchical ARD args attribute and sort if not directly using a hierarchical ARD
  if (!inherits(cards, c("ard_stack_hierarchical", "ard_stack_hierarchical_count"))) {
    attr(cards, "args") <- list(
      by = by,
      variables = variables,
      include = include
    )
    cards <-
      suppressWarnings( # suppressing warning about class
        cards |> cards::sort_ard_hierarchical("alphanumeric")
      )
  }

  brdg_hierarchical(
    cards = cards,
    variables = variables,
    by = by,
    include = include,
    statistic = statistic,
    overall_row = overall_row,
    count = FALSE,
    is_ordered = is.ordered(data[[dplyr::last(variables)]]),
    label = label
  ) |>
    append(
      list(
        cards = list(cards) |> stats::setNames("tbl_ard_hierarchical"),
        inputs = tbl_ard_hierarchical_inputs
      )
    ) |>
    structure(class = c("tbl_ard_hierarchical", "gtsummary"))
}

.check_ard_hierarchical_structure <- function(cards, variables, by, include, overall_row) {
  cards_no_attr <- cards |>
    dplyr::filter(!.data$context %in% c("attributes", "total_n"))
  group_name_cols <- cards_no_attr |>
    select(cards::all_ard_groups("names")) |>
    names()

  # every summarized variable must be present in the ARD
  missing_vars <- setdiff(union(include, dplyr::last(variables)), cards_no_attr$variable)
  if (!is_empty(missing_vars)) {
    cli::cli_abort(
      c("Results for {.val {missing_vars}} were not found in the {.arg cards} object.",
        i = "Use the {.arg include} argument to select the variables with summary statistics in the ARD."),
      call = get_cli_abort_call()
    )
  }

  # the overall row results must be present when requested
  if (overall_row && !"..ard_hierarchical_overall.." %in% cards_no_attr$variable) {
    cli::cli_abort(
      c("Argument {.code overall_row = TRUE} requires overall results in the {.arg cards} object, but they were not found.",
        i = "Use {.code over_variables = TRUE} in {.fun cards::ard_stack_hierarchical} or {.fun cards::ard_stack_hierarchical_count} to calculate them."),
      call = get_cli_abort_call()
    )
  }

  # each hierarchy variable must be nested within the `by` variable and the preceding hierarchy variables
  expected_groups <-
    lapply(seq_along(variables), \(i) c(by, variables[seq_len(i - 1L)])) |>
    stats::setNames(variables)
  if (overall_row) {
    expected_groups[["..ard_hierarchical_overall.."]] <- by
  }
  walk(
    names(expected_groups),
    \(variable) {
      df_groups <- cards_no_attr[cards_no_attr$variable %in% variable, group_name_cols, drop = FALSE] |>
        unique()
      for (i in seq_len(nrow(df_groups))) {
        observed <- unlist(df_groups[i, ], use.names = FALSE)
        observed <- observed[!is.na(observed)]
        expected <- expected_groups[[variable]] %||% character(0L)
        if (!identical(observed, expected)) {
          cli::cli_abort(
            c("The structure of the {.arg cards} object does not match the {.arg variables} and {.arg by} arguments.",
              i = "Results for variable {.val {variable}} are expected to be grouped by {if (is_empty(expected)) 'no variables' else cli::format_inline('{.val {expected}}')},
                   but are grouped by {if (is_empty(observed)) 'no variables' else cli::format_inline('{.val {observed}}')}."),
            call = get_cli_abort_call()
          )
        }
      }
    }
  )

  invisible()
}
