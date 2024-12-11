#' Summarize continuous variable
#'
#' Summarize a continuous variable by one or more categorical variables
#'
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column from `data`. Variable name of the continuous column to be summarized.
#' @param digits  ([`formula-list-selector`][syntax])\cr
#'   Specifies how summary statistics are rounded. Values may be either integer(s)
#'   or function(s). If not specified, default formatting is assigned
#'   via `assign_summary_digits()`. See below for details.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Specifies summary statistics to display for each variable.  The default is
#'   `everything() ~ "{median} ({p25}, {p75})"`.
#' @param value ([`formula-list-selector`][syntax])\cr
#'   Supply a value to display a variable on a single row, printing the
#'   results for the variable associated with the value (similar to a
#'   `'dichotomous'` display in `tbl_summary()`).
#' @inheritParams tbl_summary
#'
#' @return a gtsummary table
#' @export
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' tbl_continuous(
#'   data = trial,
#'   variable = age,
#'   by = trt,
#'   include = grade
#' )
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   dplyr::mutate(all_subjects = 1) |>
#'   tbl_continuous(
#'     variable = age,
#'     statistic = ~"{mean} ({sd})",
#'     by = trt,
#'     include = c(all_subjects, stage, grade),
#'     value = all_subjects ~ 1,
#'     label = list(all_subjects = "All Subjects")
#'   )
tbl_continuous <- function(data,
                           variable,
                           include = everything(),
                           digits = NULL,
                           by = NULL,
                           statistic = everything() ~ "{median} ({p25}, {p75})",
                           label = NULL,
                           value = NULL) {
  set_cli_abort_call()

  # data argument checks -------------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  .data_dim_checks(data)

  # process arguments ----------------------------------------------------------
  cards::process_selectors(data, by = {{ by }}, include = {{ include }}, variable = {{ variable }})
  check_scalar(variable)
  check_scalar(
    by,
    allow_empty = TRUE,
    message = c("The {.arg {arg_name}} argument must be length {.val {1}} or empty.",
                i = "Use {.fun tbl_strata} for more than one {.arg by} variable."
    )
  )
  data <- dplyr::ungroup(data) |> .drop_missing_by_obs(by = by) # styler: off
  include <- setdiff(include, c(by, variable)) # remove by and variable columns from list vars included
  data <- dplyr::ungroup(data) |> .drop_missing_by_obs(by = by) # styler: off

  # assign types and values
  cards::process_formula_selectors(data[include], value = value)
  type <- rep_named(include, list("categorical")) |>
    utils::modifyList(rep_named(names(compact(value)), list("dichotomous")))

  cards::check_list_elements(
    x = value,
    predicate = \(x) length(x) == 1L,
    error_msg =
      c("Error in argument {.arg {arg_name}} for variable {.val {variable}}.",
        "i" = "Elements values must be a scalar.")
  )


  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic = statistic,
    include_env = TRUE
  )
  # add the calling env to the statistics
  statistic <- .add_env_to_list_elements(statistic, env = caller_env())

  cards::check_list_elements(
    statistic,
    predicate = \(x) is_string(x) && !is_empty(.extract_glue_elements(x)),
    error_msg =
      c("Elements of the {.arg statistic} argument must be a string with {.arg glue} elements referring to functions.",
        i = "For example {.code statistic = list(colname = '{{mean}} ({{sd}})')}, to report the {.field mean} and {.field standard deviation}.")
  )

  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[c(include, variable)]),
    label = label
  )
  cards::check_list_elements(
    label,
    predicate = \(x) is_string(x),
    error_msg =
      c("Elements of the {.arg label} argument must be strings.")
  )

  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    digits = digits
  )

  # save processed function inputs ---------------------------------------------
  tbl_continuous_inputs <- as.list(environment())
  tbl_continuous_inputs$type <- NULL
  call <- match.call()

  # prepare the ARD data frame -------------------------------------------------
  cards <-
    map(
      include,
      \(cat_variable) {
        # convert digits input into the named lists expected by the {cards} functions
        variable_digits <-
          assign_summary_digits(
            data = data,
            statistic = statistic[cat_variable] |> set_names(variable),
            type = list("continuous") |> set_names(variable),
            digits = digits[cat_variable] |> set_names(variable)
          )

        # calculate the continuous summary stats
        cards::ard_continuous(
          data = data |> tidyr::drop_na(all_of(c(by, cat_variable))),
          variables = all_of(variable),
          by = any_of(c(by, cat_variable)),
          statistic = .continuous_statistics_chr_to_fun(statistic)[cat_variable] |> set_names(variable),
          fmt_fn = variable_digits,
          stat_label = ~ default_stat_labels()
        ) |>
          # add the missingness information
          cards::bind_ard(
            cards::ard_missing(
              data = data |> tidyr::drop_na(all_of(c(by, cat_variable))),
              variables = all_of(variable),
              by = any_of(c(by, cat_variable)),
              fmt_fn = variable_digits,
              stat_label = ~ default_stat_labels()
            )
          )
      }
    ) |>
    dplyr::bind_rows()

  # add attributes and missing information
  cards <-
    dplyr::bind_rows(
      cards,
      cards::ard_attributes(data, variables = all_of(c(variable, by, include)), label = label),
      cards::ard_categorical(data, variables = any_of(by), stat_label = ~ default_stat_labels()),
      cards::ard_total_n(data)
    )

  # subsetting ARD based on passed value ---------------------------------------
  cards <- .subset_card_based_on_value(cards, value, by)

  # fill NULL stats with NA
  cards <- cards::replace_null_statistic(cards)

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_continuous(cards, include, by)

  # prepare the base table via `brdg_continuous()` -----------------------------
  x <- brdg_continuous(cards, by = by, statistic = statistic, include = include,
                       variable = variable, type = type)

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(
      all_stat_cols() ~
        ifelse(
          is_empty(by),
          "**N = {style_number(N)}**",
          "**{level}**  \nN = {style_number(n)}"
        )
    )

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

  x |>
    structure(class = c("tbl_continuous", "gtsummary"))

  # add other information to the returned object
  x$cards <- list(tbl_continuous = cards)
  x$inputs <- tbl_continuous_inputs
  x$call_list <- list(tbl_continuous = call)

  x |>
    structure(class = c("tbl_continuous", "gtsummary"))
}


.add_gts_column_to_cards_continuous <- function(cards, variables, by) {
  if ("gts_column" %in% names(cards)) {
    cli::cli_inform("The {.val gts_column} column is alread present. Defining the column has been skipped.")
    return(cards)
  }

  # adding the name of the column the stats will populate
  if (is_empty(by)) {
    cards$gts_column <-
      ifelse(
        !cards$context %in% "attributes",
        "stat_0",
        NA_character_
      )
  } else {
    # styler: off
    cards <-
      cards %>%
      {dplyr::left_join(
        .,
        dplyr::filter(
          .,
          .data$group2 %in% .env$variables,
          !cards$context %in% "attributes",
        ) |>
          dplyr::select(cards::all_ard_groups(), "variable", "context") |>
          dplyr::distinct() |>
          dplyr::mutate(
            .by = c("group1", "group1_level"),
            gts_column = paste0("stat_", dplyr::cur_group_id())
          ),
        by = names(dplyr::select(., cards::all_ard_groups(), "variable", "context"))
      )}
    #styler: on
  }

  cards
}
