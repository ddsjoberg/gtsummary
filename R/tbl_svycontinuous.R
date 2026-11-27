#' Summarize continuous variable from a survey object
#'
#' `r lifecycle::badge('experimental')`\cr
#' Summarize a continuous variable by one or more categorical variables,
#' taking into account survey weights and design.
#'
#' @param data (`survey.design`)\cr
#'   A survey object created with created with `survey::svydesign()`
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Specifies summary statistics to display for each variable.  The default is
#'   `everything() ~ "{median} ({p25}, {p75})"`. See below for details.
#' @inheritParams tbl_continuous
#'
#' @section statistic argument:
#' The statistic argument specifies the statistics presented in the table. The
#' input is a list of formulas that specify the statistics to report. For example,
#' `statistic = list(grade ~ "{mean} ({sd})")` would report the mean and
#' standard deviation of the continuous variable within the levels of `grade`.
#' A statistic name that appears between curly brackets
#' will be replaced with the numeric statistic (see [`glue::glue()`]).
#'
#' The following statistics are available to display.
#' \itemize{
#'   \item `{median}` median
#'   \item `{mean}` mean
#'   \item `{mean.std.error}` standard error of the sample mean computed with [survey::svymean()]
#'   \item `{deff}` design effect of the sample mean computed with [survey::svymean()]
#'   \item `{sd}` standard deviation
#'   \item `{var}` variance
#'   \item `{min}` minimum
#'   \item `{max}` maximum
#'   \item `{p##}` any integer percentile, where `##` is an integer from 0 to 100
#'   \item `{sum}` sum
#' }
#'
#' Unlike [`tbl_continuous()`], it is not possible to pass a custom function.
#'
#' Statistics on the number of missing and non-missing observations of the
#' continuous variable and their proportions are also available to display.
#' \itemize{
#'   \item `{N_obs}` total number of observations
#'   \item `{N_miss}` number of missing observations
#'   \item `{N_nonmiss}` number of non-missing observations
#'   \item `{p_miss}` percentage of observations missing
#'   \item `{p_nonmiss}` percentage of observations not missing
#'   \item `{N_obs_unweighted}` unweighted total number of observations
#'   \item `{N_miss_unweighted}` unweighted number of missing observations
#'   \item `{N_nonmiss_unweighted}` unweighted number of non-missing observations
#'   \item `{p_miss_unweighted}` unweighted percentage of observations missing
#'   \item `{p_nonmiss_unweighted}` unweighted percentage of observations not missing
#' }
#'
#' @return a gtsummary table of class `'tbl_svycontinuous'`
#' @export
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("survey")
#' # Example 1 ----------------------------------
#' survey::svydesign(~1, data = trial, weights = ~1) |>
#'   tbl_svycontinuous(
#'     variable = age,
#'     by = trt,
#'     include = grade
#'   )
#'
#' # Example 2 ----------------------------------
#' # A dataset with a complex design
#' data(api, package = "survey")
#' survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
#'   tbl_svycontinuous(
#'     variable = api00,
#'     include = c(stype, awards),
#'     statistic = ~"{mean} ({sd})"
#'   )
tbl_svycontinuous <- function(data,
                              variable,
                              include = everything(),
                              digits = NULL,
                              by = NULL,
                              statistic = everything() ~ "{median} ({p25}, {p75})",
                              label = NULL,
                              value = NULL) {
  set_cli_abort_call()
  check_pkg_installed("survey")

  # data argument checks -------------------------------------------------------
  check_not_missing(data)
  check_class(data, "survey.design")
  .data_dim_checks(data$variables)

  # process arguments ----------------------------------------------------------
  cards::process_selectors(
    as.data.frame(data),
    by = {{ by }}, include = {{ include }}, variable = {{ variable }}
  )
  check_scalar(variable)
  check_scalar(
    by,
    allow_empty = TRUE,
    message = c("The {.arg {arg_name}} argument must be length {.val {1}} or empty.",
                i = "Use {.fun tbl_strata} for more than one {.arg by} variable."
    )
  )
  include <- setdiff(include, c(by, variable)) # remove by and variable columns from list vars included
  data <- .svy_ignore_missing_by_obs(data, by = by, include = c(include, variable))

  # assign types and values
  cards::process_formula_selectors(as.data.frame(data)[include], value = value)
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
    data = scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[include]),
    statistic = statistic
  )

  cards::check_list_elements(
    statistic,
    predicate = \(x) is_string(x) && !is_empty(.extract_glue_elements(x)),
    error_msg =
      c("Elements of the {.arg statistic} argument must be a string with {.arg glue} elements referring to statistics.",
        i = "For example {.code statistic = list(colname = '{{mean}} ({{sd}})')}, to report the {.field mean} and {.field standard deviation}.")
  )

  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[c(include, variable)]),
    label = label
  )
  cards::check_list_elements(
    label,
    predicate = \(x) is_string(x),
    error_msg =
      c("Elements of the {.arg label} argument must be strings.")
  )

  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[include]),
    digits = digits
  )

  # save processed function inputs ---------------------------------------------
  tbl_svycontinuous_inputs <- as.list(environment())
  tbl_svycontinuous_inputs$type <- NULL
  call <- match.call()

  # prepare the ARD data frame -------------------------------------------------
  # the missingness stats are calculated by `ard_missing()`, not `ard_summary()`
  missing_stat_names <-
    c("N_obs", "N_miss", "p_miss", "N_nonmiss", "p_nonmiss", "N_obs_unweighted",
      "N_miss_unweighted", "p_miss_unweighted", "N_nonmiss_unweighted", "p_nonmiss_unweighted")

  cards <-
    map(
      include,
      \(cat_variable) {
        # convert digits input into the named lists expected by the {cardx} functions
        variable_digits <-
          assign_summary_digits(
            data = data,
            statistic = statistic[cat_variable] |> set_names(variable),
            type = list("continuous") |> set_names(variable),
            digits = digits[cat_variable] |> set_names(variable)
          )

        # remove obs missing the categorical variable (keeps the full design)
        data_cat <- data[!is.na(data$variables[[cat_variable]]), ]

        # survey summary statistics are passed by name (no custom functions)
        variable_statistic <-
          .extract_glue_elements(statistic[[cat_variable]]) |>
          setdiff(missing_stat_names)

        cards::bind_ard(
          # calculate the continuous summary stats
          if (!is_empty(variable_statistic)) {
            cardx::ard_summary(
              data = data_cat,
              variables = all_of(variable),
              by = any_of(c(by, cat_variable)),
              statistic = list(variable_statistic) |> set_names(variable),
              fmt_fun = variable_digits,
              stat_label = ~ default_stat_labels()
            )
          },
          # add the missingness information
          .svy_ard_missing_two_by(
            data = data_cat,
            variable = variable,
            by = by,
            cat_variable = cat_variable,
            fmt_fun = variable_digits
          )
        ) |>
          .check_svycontinuous_stats_available(
            statistic = statistic[[cat_variable]],
            cat_variable = cat_variable,
            by = by
          )
      }
    ) |>
    dplyr::bind_rows()

  # add attributes and missing information
  cards <-
    dplyr::bind_rows(
      cards,
      cardx::ard_attributes(data, variables = all_of(c(variable, by, include)), label = label),
      if (!is_empty(by)) {
        cardx::ard_tabulate(data, variables = all_of(by), stat_label = ~ default_stat_labels())
      },
      cardx::ard_total_n(data)
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

  # add other information to the returned object
  x$cards <- list(tbl_svycontinuous = cards)
  x$inputs <- tbl_svycontinuous_inputs
  x$call_list <- list(tbl_svycontinuous = call)

  x |>
    structure(class = c("tbl_svycontinuous", "gtsummary"))
}

# `cardx::ard_missing.survey.design()` accepts a single `by` variable.
# When there is a `by` variable, calculate the missingness ARD by the categorical
# variable within each level of `by` (subsetting the design preserves it for
# variance estimation), and add the `by` variable as the first grouping column.
.svy_ard_missing_two_by <- function(data, variable, by, cat_variable, fmt_fun) {
  if (is_empty(by)) {
    return(
      cardx::ard_missing(
        data = data,
        variables = all_of(variable),
        by = all_of(cat_variable),
        fmt_fun = fmt_fun,
        stat_label = ~ default_stat_labels()
      )
    )
  }

  # the `by` levels, as they are represented in an ARD
  by_levels <-
    cardx::ard_missing(data, variables = all_of(variable), by = all_of(by)) |>
    dplyr::pull("group1_level") |>
    unique()

  map(
    by_levels,
    \(by_level) {
      data_by_level <- data[as.character(data$variables[[by]]) %in% as.character(by_level), ]
      ard <-
        cardx::ard_missing(
          data = data_by_level,
          variables = all_of(variable),
          by = all_of(cat_variable),
          fmt_fun = fmt_fun,
          stat_label = ~ default_stat_labels()
        ) |>
        dplyr::rename(group2 = "group1", group2_level = "group1_level")
      ard$group1 <- by
      ard$group1_level <- rep_len(list(by_level), nrow(ard))
      dplyr::relocate(ard, "group1", "group1_level", .before = "group2")
    }
  ) |>
    dplyr::bind_rows() |>
    structure(class = c("card", "tbl_df", "tbl", "data.frame"))
}

# check the requested statistics are present in the ARD for one categorical variable
.check_svycontinuous_stats_available <- function(cards, statistic, cat_variable, by) {
  extracted_stats <- .extract_glue_elements(statistic)
  available_stats <- unique(cards$stat_name)

  missing_stats <- extracted_stats[!extracted_stats %in% available_stats]
  if (!is_empty(missing_stats)) {
    cli::cli_abort(
      c("Statistic {.val {missing_stats}} is not available for variable {.val {cat_variable}}.",
        i = "Select among {.val {available_stats}}."
      ),
      call = get_cli_abort_call()
    )
  }

  cards
}
