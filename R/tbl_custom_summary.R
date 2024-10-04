#' Create a table of summary statistics using a custom summary function
#'
#' \lifecycle{experimental}\cr
#' The `tbl_custom_summary()` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.
#' This function is similar to [tbl_summary()] but allows you to provide
#' a custom function in charge of computing the statistics (see Details).
#'
#' @inheritParams tbl_summary
#' @param stat_fns ([`formula-list-selector`][syntax])\cr
#'   Specifies the function to be used to compute the statistics
#'   (see below for details and examples).
#'   You can also use dedicated helpers such as [`ratio_summary()`]
#'   or [`proportion_summary()`].
#' @param overall_row (scalar `logical`)\cr
#'   Logical indicator to display an overall row. Default is
#'   `FALSE`. Use [`add_overall()`] to add an overall column.
#' @param overall_row_last (scalar `logical`)\cr
#'   Logical indicator to display overall row last in
#'   table. Default is `FALSE`, which will display overall row first.
#' @param overall_row_label (`string`)\cr
#'   String indicating the overall row label. Default is `"Overall"`.
#'
#' @section Similarities with `tbl_summary()`:
#' Please refer to the help file of [tbl_summary()] regarding the use of select
#' helpers, and arguments `include`, `by`, `type`, `value`, `digits`, `missing` and
#' `missing_text`.
#'
#' @section `stat_fns` argument:
#' The `stat_fns` argument specify the custom function(s) to be used for computing
#' the summary statistics. For example, `stat_fns = everything() ~ foo`.
#'
#' Each function may take the following arguments:
#' `foo(data, full_data, variable, by, type, ...)`
#'
#'   - `data=` is the input data frame passed to `tbl_custom_summary()`, subset
#'     according to the level of `by` or `variable` if any, excluding `NA`
#'     values of the current `variable`
#'
#'   - `full_data=` is the full input data frame passed to `tbl_custom_summary()`
#'
#'   - `variable=` is a string indicating the variable to perform the
#'     calculation on
#'
#'   - `by=` is a string indicating the by variable from `tbl_custom_summary=`,
#'     if present
#'
#'   - `type=` is a string indicating the type of variable
#'     (continuous, categorical, ...)
#'
#'   - `stat_display=` a string indicating the statistic to display (for the
#'     `statistic` argument, for that variable)
#'
#' The user-defined does not need to utilize each of these inputs. It's
#' encouraged the user-defined function accept `...` as each of the arguments
#' *will* be passed to the function, even if not all inputs are utilized by
#' the user's function, e.g. `foo(data, ...)` (see examples).
#'
#' The user-defined function should return a one row [dplyr::tibble()] with
#' one column per summary statistics (see examples).
#'
#' @section statistic argument:
#' The statistic argument specifies the statistics presented in the table. The
#' input is a list of formulas that specify the statistics to report. For example,
#' `statistic = list(age ~ "{mean} ({sd})")`.
#' A statistic name that appears between curly brackets
#' will be replaced with the numeric statistic (see [`glue::glue()`]).
#' All the statistics indicated in the statistic argument should be returned
#' by the functions defined in the `stat_fns` argument.
#'
#' When the summary type is `"continuous2"`, pass a vector of statistics. Each element
#' of the vector will result in a separate row in the summary table.
#'
#' For both categorical and continuous variables, statistics on the number of
#' missing and non-missing observations and their proportions are also available
#' to display.
#' \itemize{
#'   \item `{N_obs}` total number of observations
#'   \item `{N_miss}` number of missing observations
#'   \item `{N_nonmiss}` number of non-missing observations
#'   \item `{p_miss}` percentage of observations missing
#'   \item `{p_nonmiss}` percentage of observations not missing
#' }
#'
#' Note that for categorical variables, `{N_obs}`, `{N_miss}` and `{N_nonmiss}` refer
#' to the total number, number missing and number non missing observations
#' in the denominator, not at each level of the categorical variable.
#'
#' It is recommended to use [`modify_footnote()`] to properly describe the
#' displayed statistics (see examples).
#'
#' @section Caution:
#'
#' The returned table is compatible with all `gtsummary` features applicable
#' to a `tbl_summary` object, like [`add_overall()`], [`modify_footnote()`] or
#' [`bold_labels()`].
#'
#' However, some of them could be inappropriate in such case. In particular,
#' [`add_p()`] do not take into account the type of displayed statistics and
#' always return the p-value of a comparison test of the current variable
#' according to the `by` groups, which may be incorrect if the displayed
#' statistics refer to a third variable.
#'
#' @export
#'
#' @return A `tbl_custom_summary` object
#' @author Joseph Larmarange
#' @examples
#' # Example 1 ----------------------------------
#' my_stats <- function(data, ...) {
#'   marker_sum <- sum(data$marker, na.rm = TRUE)
#'   mean_age <- mean(data$age, na.rm = TRUE)
#'   dplyr::tibble(
#'     marker_sum = marker_sum,
#'     mean_age = mean_age
#'   )
#' }
#'
#' my_stats(trial)
#'
#' trial |>
#'   tbl_custom_summary(
#'     include = c("stage", "grade"),
#'     by = "trt",
#'     stat_fns = everything() ~ my_stats,
#'     statistic = everything() ~ "A: {mean_age} - S: {marker_sum}",
#'     digits = everything() ~ c(1, 0),
#'     overall_row = TRUE,
#'     overall_row_label = "All stages & grades"
#'   ) |>
#'   add_overall(last = TRUE) |>
#'   modify_footnote(
#'     all_stat_cols() ~ "A: mean age - S: sum of marker"
#'   ) |>
#'   bold_labels()
#'
#' # Example 2 ----------------------------------
#' # Use `data[[variable]]` to access the current variable
#' mean_ci <- function(data, variable, ...) {
#'   test <- t.test(data[[variable]])
#'   dplyr::tibble(
#'     mean = test$estimate,
#'     conf.low = test$conf.int[1],
#'     conf.high = test$conf.int[2]
#'   )
#' }
#'
#' trial |>
#'   tbl_custom_summary(
#'     include = c("marker", "ttdeath"),
#'     by = "trt",
#'     stat_fns = ~ mean_ci,
#'     statistic = ~ "{mean} [{conf.low}; {conf.high}]"
#'   ) |>
#'   add_overall(last = TRUE) |>
#'   modify_footnote(
#'     all_stat_cols() ~ "mean [95% CI]"
#'   )
#'
#' # Example 3 ----------------------------------
#' # Use `full_data` to access the full datasets
#' # Returned statistic can also be a character
#' diff_to_great_mean <- function(data, full_data, ...) {
#'   mean <- mean(data$marker, na.rm = TRUE)
#'   great_mean <- mean(full_data$marker, na.rm = TRUE)
#'   diff <- mean - great_mean
#'   dplyr::tibble(
#'     mean = mean,
#'     great_mean = great_mean,
#'     diff = diff,
#'     level = ifelse(diff > 0, "high", "low")
#'   )
#' }
#'
#' trial |>
#'   tbl_custom_summary(
#'     include = c("grade", "stage"),
#'     by = "trt",
#'     stat_fns = ~ diff_to_great_mean,
#'     statistic = ~ "{mean} ({level}, diff: {diff})",
#'     overall_row = TRUE
#'   ) |>
#'   bold_labels()
tbl_custom_summary <- function(data,
                               by = NULL,
                               label = NULL,
                               stat_fns,
                               statistic,
                               digits = NULL,
                               type = NULL,
                               value = NULL,
                               missing = c("ifany", "no", "always"),
                               missing_text = "Unknown",
                               missing_stat = "{N_miss}",
                               include = everything(),
                               overall_row = FALSE,
                               overall_row_last = FALSE,
                               overall_row_label = "Overall") {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_data_frame(data)
  .data_dim_checks(data)
  check_scalar_logical(overall_row)
  check_scalar_logical(overall_row_last)
  if (missing(overall_row_label)) overall_row_label <- translate_string(overall_row_label)
  check_string(overall_row_label)

  if (missing(missing)) {
    missing <-
      get_theme_element("tbl_custom_summary-arg:missing") %||%
      get_theme_element("tbl_summary-arg:missing", default = missing)
  }
  missing <- arg_match(missing, values = c("ifany", "no", "always"))

  if (missing(missing_text)) {
    missing_text <-
      get_theme_element("tbl_custom_summary-arg:missing_text") %||%
      get_theme_element("tbl_summary-arg:missing_text", default = translate_string(missing_text))
  }
  check_string(missing_text)

  # process arguments ----------------------------------------------------------
  cards::process_selectors(data, by = {{ by }}, include = {{ include }})
  check_scalar(
    by,
    allow_empty = TRUE,
    message = c("The {.arg {arg_name}} argument must be length {.val {length}} or empty.",
                i = "Use {.fun tbl_strata} for more than one {.arg by} variable."
    )
  )
  data <- dplyr::ungroup(data) |> .drop_missing_by_obs(by = by) # styler: off
  include <- setdiff(include, by) # remove by variable from list vars included

  # Adding overall if requested
  if (overall_row) {
    data$.overall <- TRUE
    attr(data$.overall, "label") <- overall_row_label

    include <- append(include, ".overall", after = ifelse(overall_row_last, length(include), 0))
  }

  cards::process_formula_selectors(
    data = data[include],
    value =
      case_switch(
        missing(value) ~
          get_theme_element("tbl_custom_summary-arg:value") %||%
          get_theme_element("tbl_summary-arg:value", default = value),
        .default = value
      )
  )

  # assign summary type --------------------------------------------------------
  if (!is_empty(type)) {
    # first set default types, so selectors like `all_continuous()` can be used
    # to recast the summary type, e.g. make all continuous type "continuous2"
    default_types <- assign_summary_type(data, include, value)
    # process the user-passed type argument
    cards::process_formula_selectors(
      data = scope_table_body(.list2tb(default_types, "var_type"), data[include]),
      type =
        case_switch(
          missing(type) ~
            get_theme_element("tbl_custom_summary-arg:type") %||%
            get_theme_element("tbl_summary-arg:type", default = type),
          .default = type
        )
    )
    # fill in any types not specified by user
    type <- utils::modifyList(default_types, type)
  } else {
    type <- assign_summary_type(data, include, value)
  }

  value <-
    scope_table_body(.list2tb(type, "var_type"), data[include]) |>
    .assign_default_values(value, type)

  # evaluate the remaining list-formula arguments ------------------------------
  # processed arguments are saved into this env
  cards::process_formula_selectors(
    scope_table_body(.list2tb(type, "var_type"), data[include]),
    statistic =
      case_switch(
        missing(statistic) ~ get_theme_element("tbl_summary-arg:statistic", default = statistic),
        .default = statistic
      ),
    stat_fns = stat_fns
  )
  cards::check_list_elements(
    x = stat_fns,
    predicate = \(x) is.function(x),
    error_msg = "Each value passed in the {.arg stat_fns} argument must be a function."
  )

  scope_table_body(.list2tb(type, "var_type"), data[include]) |>
    cards::process_formula_selectors(
      label =
        case_switch(
          missing(label) ~
            get_deprecated_theme_element("tbl_custom_summary-arg:label") %||%
            get_deprecated_theme_element("tbl_summary-arg:label", default = label),
          .default = label
        )
    )


  cards::process_formula_selectors(
    data = scope_table_body(.list2tb(rep_named(include, list("continuous")), "var_type"), data[include]),
    digits =
      case_switch(
        missing(digits) ~
          get_theme_element("tbl_custom_summary-arg:digits") %||%
          get_theme_element("tbl_summary-arg:digits", default = digits),
        .default = digits
      )
  )
  user_passed_digits <- digits

  # fill each element of digits argument
  if (!missing(digits)) {
    digits <-
      scope_table_body(.list2tb(rep_named(include, list("continuous")), "var_type"), data[include]) |>
      assign_summary_digits(statistic, rep_named(include, list("continuous")), digits = digits)
  }

  # more checks on inputs ------------------------------------------------------
  .check_haven_labelled(data[c(include, by)])
  .check_tbl_summary_args(
    data = data, label = label, statistic = statistic,
    digits = digits, type = type, value = value
  )

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_custom_summary_inputs <- as.list(environment())
  tbl_custom_summary_inputs[["default_types"]] <- NULL
  call <- match.call()


  # construct cards ------------------------------------------------------------
  variables_categorical <- keep(type, \(x) x %in% c("categorical", "dichotomous")) |> names()
  ard_categorical <-
    if (is_empty(variables_categorical)) dplyr::tibble() # styler: off
  else {
    map(
      variables_categorical,
      \(variable) {
        ard <-
          cards::ard_complex(
            data |> tidyr::drop_na(all_of(variable)),
            variables = all_of(variable),
            by = all_of(c(by, variable)),
            statistic = stat_fns[variable] |> map(~list(complex = .x)),
            fmt_fn = digits[variable]
          ) |>
          dplyr::select(-cards::all_ard_variables()) |>
          dplyr::mutate(context = type[[variable]])

        # rename columns to align with expectations similar to `tbl_summary()` results
        ard <-
          case_switch(
            !is_empty(by) ~ dplyr::rename(ard, variable = "group2", variable_level = "group2_level"),
            .default = dplyr::rename(ard, variable = "group1", variable_level = "group1_level")
          )
      }
    ) |>
      cards::bind_ard() |>
      cards::tidy_ard_column_order() |>
      # for dichotomous, keep the value of interest
      dplyr::filter(
        map2_lgl(
          .data$variable, .data$variable_level,
          ~type[[.x]] == "categorical" | (type[[.x]] == "dichotomous" & .y %in% value[[.x]])
        )
      )
  }


  variables_continuous <- keep(type, \(x) x %in% c("continuous", "continuous2")) |> names()
  ard_continuous <-
    cards::ard_complex(
      data,
      variables = all_of(variables_continuous),
      by = any_of(by),
      statistic = stat_fns[variables_continuous] |> map(~list(complex = .x)),
      fmt_fn = digits[variables_continuous]
    ) |>
    dplyr::mutate(context = "continuous")

  # construct cards ------------------------------------------------------------
  cards <-
    cards::bind_ard(
      cards::ard_attributes(data, variables = all_of(c(include, by)), label = label),
      cards::ard_missing(data,
                         variables = all_of(include),
                         by = all_of(by),
                         fmt_fn = digits,
                         stat_label = ~ default_stat_labels()
      ),
      cards::ard_total_n(data),
      # tabulate by variable for header stats
      if (!is_empty(by)) {
        cards::ard_categorical(data,
                               variables = all_of(by),
                               stat_label = ~ default_stat_labels()
        )
      },
      ard_continuous,
      ard_categorical
    ) |>
    cards::replace_null_statistic()


  # fixing integer fmt_fn that have defaulted to character/date results
  # cycle through the formatting functions, and replace default numeric for character stats
  # THIS IS NOT PERFECT! IN v2.0, WE NOW LET USERS PASS A SINGLE FMTFN TO A SINGLE
  #   STAT USING NAMED LISTS (previously, each fn had to be specified if you specified one).
  #   NOW IF A USER PASSES A SINGLE FMTN, THIS WILL NOT TRIGGER THE CHANGE TO
  #   CHARACTER AND DATE FMTFNs
  for (i in seq_len(nrow(cards))) {
    stat <- cards$stat[i][[1]]
    variable <- cards$variable[i]
    stat_name <- cards$stat_name[i]

    if (inherits(stat, c("character", "Date")) &&                  # if the stat is character OR Date
        is_empty(digits[names(user_passed_digits)][[variable]][[stat_name]]) &&  # and the user didn't pass a digit for this stat
        !is_empty(digits[[variable]][[stat_name]]) ) {                           # and we created a default formatting stat, THEN change the default to as.character()
      cards$fmt_fn[i][[1]] <- as.character
      digits[[variable]][[stat_name]] <- as.character
    }
  }
  tbl_custom_summary_inputs$digits <- digits
  tbl_custom_summary_inputs$user_passed_digits <- NULL

  # print all warnings and errors that occurred while calculating requested stats
  cards::print_ard_conditions(cards)

  # check the requested stats are present in ARD data frame
  .check_stats_available(cards = cards, statistic = statistic)

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
        cards = list(tbl_custom_summary = cards),
        inputs = tbl_custom_summary_inputs
      )
    ) |>
    structure(class = c("tbl_custom_summary", "tbl_summary", "gtsummary"))

  # adding styling -------------------------------------------------------------
  x <- x |>
    # add header to label column and add default indentation
    modify_table_styling(
      columns = "label",
      label = glue("**{translate_string('Characteristic')}**"),
      rows = .data$row_type %in% c("level", "missing"),
      indent = 4L
    ) |>
    # adding the statistic footnote
    modify_table_styling(
      columns = all_stat_cols(),
      footnote =
        .construct_summary_footnote(x$cards[["tbl_custom_summary"]], x$inputs$include, x$inputs$statistic, x$inputs$type)
    ) |>
    # updating the headers for the stats columns
    modify_header(
      all_stat_cols() ~
        ifelse(
          is_empty(by),
          get_theme_element("tbl_summary-str:header-noby",
                            default = "**N = {style_number(N)}**"),
          get_theme_element("tbl_summary-str:header-withby",
                            default = "**{level}**  \nN = {style_number(n)}")
        )
    )

  # return tbl_summary table ---------------------------------------------------
  x$call_list <- list(tbl_custom_summary = call)
  x
}
