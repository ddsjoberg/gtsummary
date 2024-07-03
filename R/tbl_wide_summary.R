#' Wide summary table
#'
#' `r lifecycle::badge("experimental")`\cr
#' This function is similar to `tbl_summary()`, but places summary statistics
#' wide, in separate columns.
#' All included variables must be of the same summary type, e.g. all continuous
#' summaries or all categorical summaries (which encompasses dichotomous variables).
#'
#' @inheritParams tbl_summary
#' @param statistic (`character`)\cr
#'   character vector of the statistics to present. Each element of the vector
#'   will result in a column in the summary table. Default is
#'   `c("{median}", "{p25}, {p75}")` for continuous summaries, and
#'   `c("{n}", "{p}%")` for categorical/dichotomous summaries
#'
#' @return a gtsummary table of class 'tbl_wide_summary'
#' @export
#'
#' @examples
#' trial |>
#'   tbl_wide_summary(include = c(response, grade))
#'
#' trial |>
#'   tbl_strata(
#'     strata = trt,
#'     ~tbl_wide_summary(.x, include = c(age, marker))
#'   )
tbl_wide_summary <- function(data,
                             label = NULL,
                             statistic =
                               switch(type[[1]],
                                      "continuous" = c("{median}", "{p25}, {p75}"),
                                      c("{n}", "{p}%")),
                             digits = NULL,
                             type = NULL,
                             value = NULL,
                             sort = all_categorical(FALSE) ~ "alphanumeric",
                             include = everything()) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  check_not_missing(data)
  check_class(data, "data.frame")
  data <- dplyr::ungroup(data) |> .drop_missing_by_obs(by = NULL) # styler: off

  cards::process_selectors(data, include = {{ include }})

  cards::process_formula_selectors(data = data[include], value = value)

  # assign summary type --------------------------------------------------------
  if (!is_empty(type)) {
    # first set default types, so selectors like `all_continuous()` can be used
    # to recast the summary type, e.g. make all continuous type "continuous2"
    default_types <- assign_summary_type(data, include, value)
    # process the user-passed type argument
    cards::process_formula_selectors(
      data = scope_table_body(.list2tb(default_types, "var_type"), data[include]),
      type = type
    )
    # fill in any types not specified by user
    type <- utils::modifyList(default_types, type)
  } else {
    type <- assign_summary_type(data, include, value)
  }

  # summary types must be all 'continuous' OR c('categorical', 'dichotomous')
  if (!all(unlist(type) %in% "continuous") && !all(unlist(type) %in% c("categorical", "dichotomous"))) {
    cli::cli_abort(
      "The summary types must all be {.val continuous} or all be {.val {c('categorical', 'dichotomous')}}.",
      call = get_cli_abort_call()
    )
  }
  check_class(statistic, "character")
  statistic <- rep_named(include, list(statistic))

  value <-
    scope_table_body(.list2tb(type, "var_type"), data[include]) |>
    .assign_default_values(value, type)

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic = statistic,
    include_env = TRUE
  )

  # add the calling env to the statistics
  statistic <- .add_env_to_list_elements(statistic, env = caller_env())

  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(type, "var_type"), data[include]),
    label = label,
    sort = sort,
    digits = digits
  )

  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    sort = eval(formals(gtsummary::tbl_wide_summary)[["sort"]]),
  )

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <-
      scope_table_body(.list2tb(type, "var_type"), data[include]) |>
      assign_summary_digits(statistic, type, digits = digits)
  }

  # check inputs ---------------------------------------------------------------
  .check_haven_labelled(data[include])
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = digits, type = type, value = value, sort = sort
  )

  # sort requested columns by frequency
  data <- .sort_data_infreq(data, sort)

  # save processed function inputs ---------------------------------------------
  tbl_wide_summary_inputs <-
    as.list(environment()) |>
    utils::modifyList(list(default_types = NULL))
  call <- match.call()

  # construct cards ------------------------------------------------------------
  cards <-
    cards::bind_ard(
      cards::ard_attributes(data, variables = all_of(include), label = label),
      cards::ard_missing(data,
                         variables = all_of(include),
                         fmt_fn = digits,
                         stat_label = ~ default_stat_labels()
      ),
      # tabulate categorical summaries
      cards::ard_categorical(
        scope_table_body(.list2tb(type, "var_type"), data),
        variables = all_categorical(FALSE),
        fmt_fn = digits,
        denominator = "column",
        stat_label = ~ default_stat_labels()
      ),
      # tabulate dichotomous summaries
      cards::ard_dichotomous(
        scope_table_body(.list2tb(type, "var_type"), data),
        variables = all_dichotomous(),
        fmt_fn = digits,
        denominator = "column",
        value = value,
        stat_label = ~ default_stat_labels()
      ),
      # calculate continuous summaries
      cards::ard_continuous(
        scope_table_body(.list2tb(type, "var_type"), data),
        variables = all_continuous(),
        statistic =
          .continuous_statistics_chr_to_fun(
            statistic[select(scope_table_body(.list2tb(type, "var_type"), data), all_continuous()) |> names()]
          ),
        fmt_fn = digits,
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

  # construct initial tbl_summary object ---------------------------------------
  x <-
    brdg_wide_summary(
      cards = cards,
      variables = include,
      statistic = statistic,
      type = type
    ) |>
    append(
      list(
        cards = list(tbl_wide_summary = cards),
        inputs = tbl_wide_summary_inputs,
        call_list <- list(tbl_wide_summary = call)
      )
    ) |>
    structure(class = c("tbl_wide_summary", "gtsummary"))

  # return tbl_summary table ---------------------------------------------------
  x
}
