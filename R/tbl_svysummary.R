#' Create a table of summary statistics from a survey object
#'
#' The `tbl_svysummary()` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables taking into account survey weights and design.
#'
#' @param data (`survey.design`)\cr
#'   A survey object created with created with `survey::svydesign()`
#' @inheritParams tbl_summary
#'
#' @inheritSection tbl_summary type and value arguments
#'
#' @section statistic argument:
#' The statistic argument specifies the statistics presented in the table. The
#' input is a list of formulas that specify the statistics to report. For example,
#' `statistic = list(age ~ "{mean} ({sd})")` would report the mean and
#' standard deviation for age; `statistic = list(all_continuous() ~ "{mean} ({sd})")`
#' would report the mean and standard deviation for all continuous variables.
#'  A statistic name that appears between curly brackets
#' will be replaced with the numeric statistic (see [`glue::glue()`]).
#'
#' For categorical variables the following statistics are available to display.
#' \itemize{
#'   \item `{n}` frequency
#'   \item `{N}` denominator, or cohort size
#'   \item `{p}` proportion
#'   \item `{p.std.error}` standard error of the sample proportion (on the 0 to 1 scale) computed with [survey::svymean()]
#'   \item `{deff}` design effect of the sample proportion computed with [survey::svymean()]
#'   \item `{n_unweighted}` unweighted frequency
#'   \item `{N_unweighted}` unweighted denominator
#'   \item `{p_unweighted}` unweighted formatted percentage
#' }
#' For continuous variables the following statistics are available to display.
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
#' Unlike [`tbl_summary()`], it is not possible to pass a custom function.
#'
#' For both categorical and continuous variables, statistics on the number of
#' missing and non-missing observations and their proportions are available to
#' display.
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
#' Note that for categorical variables, `{N_obs}`, `{N_miss}` and `{N_nonmiss}` refer
#' to the total number, number missing and number non missing observations
#' in the denominator, not at each level of the categorical variable.
#'
#' @export
#' @return A `'tbl_svysummary'` object
#'
#' @author Joseph Larmarange
#' @examplesIf gtsummary:::is_pkg_installed(c("cardx", "survey"))
#' # Example 1 ----------------------------------
#' survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
#'   tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))
#'
#' # Example 2 ----------------------------------
#' # A dataset with a complex design
#' data(api, package = "survey")
#' survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
#'   tbl_svysummary(by = "both", include = c(api00, stype)) |>
#'   modify_spanning_header(all_stat_cols() ~ "**Survived**")
tbl_svysummary <- function(data,
                           by = NULL,
                           label = NULL,
                           statistic = list(
                             all_continuous() ~ "{median} ({p25}, {p75})",
                             all_categorical() ~ "{n} ({p}%)"
                           ),
                           digits = NULL,
                           type = NULL,
                           value = NULL,
                           missing = c("ifany", "no", "always"),
                           missing_text = "Unknown",
                           missing_stat = "{N_miss}",
                           sort = all_categorical(FALSE) ~ "alphanumeric",
                           percent = c("column", "row", "cell"),
                           include = everything()) {
  set_cli_abort_call()
  check_pkg_installed(c("cardx", "survey"))

  # data argument checks -------------------------------------------------------
  check_not_missing(data)
  check_class(data, "survey.design")
  .data_dim_checks(data$variables)

  # process arguments ----------------------------------------------------------
  cards::process_selectors(as.data.frame(data), by = {{ by }}, include = {{ include }})
  check_scalar(
    by,
    allow_empty = TRUE,
    message = c("The {.arg {arg_name}} argument must be length {.val {1}} or empty.",
                i = "Use {.fun tbl_strata} for more than one {.arg by} variable."
    )
  )

  data$variables <- .drop_missing_by_obs(data$variables, by = by) # styler: off
  include <- setdiff(include, by) # remove by variable from list vars included


  if (missing(missing)) {
    missing <-
      get_theme_element("tbl_svysummary-arg:missing") %||%
      get_theme_element("tbl_summary-arg:missing", default = missing)
  }
  missing <- arg_match(missing, values = c("ifany", "no", "always"))

  if (missing(missing_text)) {
    missing_text <- get_theme_element("tbl_svysummary-arg:missing_text") %||%
      get_theme_element("tbl_summary-arg:missing_text", default = translate_string(missing_text)) # styler: off
  }
  check_string(missing_text)

  if (missing(percent)) {
    percent <- get_theme_element("tbl_svysummary-arg:percent") %||%
      get_theme_element("tbl_summary-arg:percent", default = percent)
  }
  percent <- arg_match(percent, values = c("column", "row", "cell"))

  cards::process_formula_selectors(
    data = as.data.frame(data)[include],
    value =
      case_switch(
        missing(value) ~
          get_theme_element("tbl_svysummary-arg:value") %||%
          get_theme_element("tbl_summary-arg:value", default = value),
        .default = value
      )
  )

  # assign summary type --------------------------------------------------------
  if (!is_empty(type)) {
    # first set default types, so selectors like `all_continuous()` can be used
    # to recast the summary type, e.g. make all continuous type "continuous2"
    default_types <- assign_summary_type(as.data.frame(data), include, value)
    # process the user-passed type argument
    cards::process_formula_selectors(
      data = scope_table_body(.list2tb(default_types, "var_type"), as.data.frame(data)[include]),
      type =
        case_switch(
          missing(type) ~
            get_theme_element("tbl_svysummary-arg:type") %||%
            get_theme_element("tbl_summary-arg:type", default = type),
          .default = type
        )
    )
    # fill in any types not specified by user
    type <- utils::modifyList(default_types, type)
  } else {
    type <- assign_summary_type(as.data.frame(data), include, value)
  }

  value <-
    scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[include]) |>
    .assign_default_values(value, type)


  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[include]),
    statistic =
      case_switch(
        missing(statistic) ~
          get_theme_element("tbl_svysummary-arg:statistic") %||%
          get_theme_element("tbl_summary-arg:statistic", default = statistic),
        .default = statistic
      )
  )

  scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[include]) |>
    cards::process_formula_selectors(
      label =
        case_switch(
          missing(label) ~
            get_deprecated_theme_element("tbl_svysummary-arg:label") %||%
            get_deprecated_theme_element("tbl_summary-arg:label", default = label),
          .default = label
        ),
      sort =
        case_switch(
          missing(sort) ~
            get_theme_element("tbl_svysummary-arg:sort") %||%
            get_theme_element("tbl_summary-arg:sort", default = sort),
          .default = sort
        )
    )

  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[include]),
    digits =
      case_switch(
        missing(digits) ~
          get_theme_element("tbl_svysummary-arg:digits") %||%
          get_theme_element("tbl_summary-arg:digits", default = digits),
        .default = digits
      )
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), as.data.frame(data)[include]),
    statistic =
      get_theme_element("tbl_svysummary-arg:statistic") %||%
      get_theme_element("tbl_summary-arg:statistic", default = eval(formals(gtsummary::tbl_svysummary)[["statistic"]])),
    sort =
      get_theme_element("tbl_svysummary-arg:sort") %||%
      get_theme_element("tbl_summary-arg:sort", default = eval(formals(gtsummary::tbl_svysummary)[["sort"]])),
    digits =
      get_theme_element("tbl_svysummary-arg:digits") %||%
      get_theme_element("tbl_summary-arg:digits", default = eval(formals(gtsummary::tbl_svysummary)[["digits"]]))
  )

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <-
      data |>
      assign_summary_digits(statistic, type, digits = digits)
  }

  # check inputs ---------------------------------------------------------------
  check_string(missing_stat)
  check_string(missing_text)
  .check_haven_labelled(as.data.frame(data)[c(include, by)])
  .check_tbl_summary_args(
    data = as.data.frame(data), label = label, statistic = statistic,
    digits = digits, type = type, value = value, sort = sort
  )
  .check_statistic_type_agreement(statistic, type)

  # sort requested columns by frequency
  data$variables <- .sort_data_infreq(data$variables, sort)

  # save processed function inputs ---------------------------------------------
  tbl_svysummary_inputs <-
    as.list(environment()) |>
    utils::modifyList(list(default_types = NULL))
  call <- match.call()

  # construct cards ------------------------------------------------------------
  variables_continuous <- type |> keep(~.x %in% c("continuous", "continuous2")) |> names()
  variables_categorical <- type |> keep(~.x %in% "categorical") |> names()
  variables_dichotomous <- type |> keep(~.x %in% "dichotomous") |> names()
  statistic_continuous <-
    statistic[variables_continuous] |>
    lapply(.extract_glue_elements) |>
    map(~.x |> setdiff(c("N_obs", "N_miss", "p_miss", "N_nonmiss", "p_nonmiss", "N_obs_unweighted",
                         "N_miss_unweighted", "p_miss_unweighted", "N_nonmiss_unweighted", "p_nonmiss_unweighted"))) |>
    compact()
  # if a user only requests missingness stats, there are no "continuous" stats to calculate
  variables_continuous <- intersect(variables_continuous, names(statistic_continuous))

  cards <-
    cards::bind_ard(
      # attributes for summary columns
      cardx::ard_attributes(data, variables = all_of(c(include, by)), label = label),
      # total N
      cardx::ard_total_n(data),
      # tabulate missing information
      cardx::ard_missing(data,
                         variables = all_of(include),
                         by = all_of(by),
                         fmt_fn = digits,
                         stat_label = ~ default_stat_labels()),
      # tabulate by variable for header stats
      if (!is_empty(by)) {
        cardx::ard_categorical(data,
                               variables = all_of(by),
                               stat_label = ~ default_stat_labels())
      },
      # tabulate categorical summaries
      cardx::ard_categorical(
        data,
        by = all_of(by),
        variables = all_of(variables_categorical),
        fmt_fn = digits[variables_categorical],
        denominator = percent,
        stat_label = ~ default_stat_labels()
      ),
      # tabulate dichotomous summaries
      cardx::ard_dichotomous(
        data,
        by = all_of(by),
        variables = all_of(variables_dichotomous),
        fmt_fn = digits[variables_dichotomous],
        denominator = percent,
        value = value[variables_dichotomous],
        stat_label = ~ default_stat_labels()
      ),
      # calculate continuous summaries
      cardx::ard_continuous(
        data,
        by = all_of(by),
        variables = all_of(variables_continuous),
        statistic = statistic_continuous,
        fmt_fn = digits[variables_continuous],
        stat_label = ~ default_stat_labels()
      )
    ) |>
    cards::replace_null_statistic()

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # check the requested stats are present in ARD data frame
  .check_stats_available(cards = cards, statistic = statistic)

  # translate statistic labels -------------------------------------------------
  cards$stat_label <- translate_vector(cards$stat_label)

  # add the gtsummary column names to ARD data frame ---------------------------
  cards <- .add_gts_column_to_cards_summary(cards, include, by)

  # construct initial tbl_summary object ---------------------------------------
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
        cards = list(tbl_svysummary = cards),
        inputs = tbl_svysummary_inputs
      )
    ) |>
    structure(class = c("tbl_svysummary", "gtsummary"))

  # adding styling -------------------------------------------------------------
  x <- x |>
    # updating the headers for the stats columns
    modify_header(
      all_stat_cols() ~
        ifelse(
          is_empty(by),
          get_theme_element("tbl_svysummary-str:header-noby",
                            default = "**N = {style_number(N)}**"),
          get_theme_element("tbl_svysummary-str:header-withby",
                            default = "**{level}**  \nN = {style_number(n)}")
        )
    )

  # return tbl_summary table ---------------------------------------------------
  x$call_list <- list(tbl_svysummary = call)
  # running any additional mods
  x <-
    get_theme_element("tbl_svysummary-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}

