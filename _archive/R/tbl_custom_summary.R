#' Create a table of summary statistics using a custom summary function
#'
#' \lifecycle{experimental}
#' The `tbl_custom_summary()` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.
#' This function is similar to [tbl_summary()] but allows you to provide
#' a custom function in charge of computing the statistics (see Details).
#'
#' @inheritParams tbl_summary
#' @param stat_fns Formula or list of formulas specifying the function to be
#' used to compute the statistics (see below for details and examples). You can
#' also use dedicated helpers such as [continuous_summary()], [ratio_summary()]
#' or [proportion_summary()].
#' @param statistic List of formulas specifying the [glue::glue()] pattern to
#' display the statistics for each variable. The statistics should be returned
#' by the functions specified in `stat_fns` (see below for details and
#' examples).
#' @param overall_row Logical indicator to display an overall row. Default is
#' `FALSE`. Use [add_overall()] to add an overall column.
#' @param overall_row_last Logical indicator to display overall row last in
#' table. Default is `FALSE`, which will display overall row first.
#' @param overall_row_label String indicating the overall row label. Default is
#' `"Overall"`.
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
#' will be replaced with the numeric statistic (see [glue::glue()]).
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
#' It is recommended to use [modify_footnote()] to properly describe the
#' displayed statistics (see examples).
#'
#' @section Caution:
#'
#' The returned table is compatible with all `gtsummary` features applicable
#' to a `tbl_summary` object, like [add_overall()], [modify_footnote()] or
#' [bold_labels()].
#'
#' However, some of them could be inappropriate in such case. In particular,
#' [add_p()] do not take into account the type of displayed statistics and
#' always return the p-value of a comparison test of the current variable
#' according to the `by` groups, which may be incorrect if the displayed
#' statistics refer to a third variable.
#'
#' @export
#' @family tbl_summary tools
#' @family tbl_custom_summary tools
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @return A `tbl_custom_summary` and `tbl_summary` object
#' @author Joseph Larmarange
#' @examples
#' \donttest{
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
#' tbl_custom_summary_ex1 <-
#'   trial %>%
#'   tbl_custom_summary(
#'     include = c("stage", "grade"),
#'     by = "trt",
#'     stat_fns = everything() ~ my_stats,
#'     statistic = everything() ~ "A: {mean_age} - S: {marker_sum}",
#'     digits = everything() ~ c(1, 0),
#'     overall_row = TRUE,
#'     overall_row_label = "All stages & grades"
#'   ) %>%
#'   add_overall(last = TRUE) %>%
#'   modify_footnote(
#'     update = all_stat_cols() ~ "A: mean age - S: sum of marker"
#'   ) %>%
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
#' tbl_custom_summary_ex2 <-
#'   trial %>%
#'   tbl_custom_summary(
#'     include = c("marker", "ttdeath"),
#'     by = "trt",
#'     stat_fns = ~mean_ci,
#'     statistic = ~"{mean} [{conf.low}; {conf.high}]"
#'   ) %>%
#'   add_overall(last = TRUE) %>%
#'   modify_footnote(
#'     update = all_stat_cols() ~ "mean [95% CI]"
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
#' tbl_custom_summary_ex3 <-
#'   trial %>%
#'   tbl_custom_summary(
#'     include = c("grade", "stage"),
#'     by = "trt",
#'     stat_fns = ~diff_to_great_mean,
#'     statistic = ~"{mean} ({level}, diff: {diff})",
#'     overall_row = TRUE
#'   ) %>%
#'   bold_labels()
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_custom_summary_ex1.png", width = "45")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_custom_summary_ex2.png", width = "45")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_custom_summary_ex3.png", width = "35")`
#' }}

tbl_custom_summary <- function(data, by = NULL, label = NULL,
                               stat_fns, statistic,
                               digits = NULL, type = NULL, value = NULL,
                               missing = NULL, missing_text = NULL,
                               include = everything(),
                               overall_row = FALSE,
                               overall_row_last = FALSE,
                               overall_row_label = NULL) {
  # ungrouping data ------------------------------------------------------------
  data <- data %>% ungroup()

  # eval -----------------------------------------------------------------------
  by <-
    .select_to_varnames(
      select = {{ by }},
      data = data,
      arg_name = "by",
      select_single = TRUE
    )
  include <-
    .select_to_varnames(
      select = {{ include }},
      data = data,
      arg_name = "include"
    ) %>%
    union(by) # include by variable by default

  # adding overall row? --------------------------------------------------------
  if (overall_row) {
    if (is.null(overall_row_label)) {
      overall_row_label <- translate_text("Overall")
    }
    data$.overall <- TRUE
    attr(data$.overall, "label") <- overall_row_label

    if (overall_row_last) {
      include <- c(include, ".overall")
    } else {
      include <- c(".overall", include)
    }
  }

  # setting defaults from gtsummary theme --------------------------------------
  label <- label %||%
    get_theme_element("tbl_custom_summary-arg:label") %||%
    get_theme_element("tbl_summary-arg:label")
  digits <- digits %||%
    get_theme_element("tbl_custom_summary-arg:digits") %||%
    get_theme_element("tbl_summary-arg:digits")
  type <- type %||%
    get_theme_element("tbl_custom_summary-arg:type") %||%
    get_theme_element("tbl_summary-arg:type")
  value <- value %||%
    get_theme_element("tbl_custom_summary-arg:value") %||%
    get_theme_element("tbl_summary-arg:value")
  missing <-
    missing %||%
    get_theme_element("tbl_custom_summary-arg:missing") %||%
    get_theme_element("tbl_summary-arg:missing", default = "ifany")
  missing_text <-
    missing_text %||%
    get_theme_element("tbl_custom_summary-arg:missing_text") %||%
    get_theme_element("tbl_summary-arg:missing_text",
      default = translate_text("Unknown")
    )

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing, choices = c("ifany", "always", "no"))

  # checking input data --------------------------------------------------------
  tbl_summary_data_checks(data)

  # deleting obs with missing by values ----------------------------------------
  # saving variable labels
  if (!is.null(by) && sum(is.na(data[[by]])) > 0) {
    message(glue(
      "{sum(is.na(data[[by]]))} observations missing `{by}` have been removed. ",
      "To include these observations, use `forcats::fct_na_value_to_level()` on `{by}` ",
      "column before passing to `tbl_summary()`."
    ))

    data <- filter(data, !is.na(.data[[by]]))
  }

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_custom_summary_inputs <- as.list(environment())

  # checking function inputs ---------------------------------------------------
  tbl_summary_input_checks(data, by, missing_text, include)

  # generate meta_data --------------------------------------------------------
  meta_data <- generate_metadata_custom_summary(
    data = data, stat_fns = stat_fns, include = include,
    value = value, by = by,
    type = type, label = label, statistic = statistic,
    digits = digits
  )

  # calculating summary statistics ---------------------------------------------
  table_body <-
    meta_data %>%
    mutate(
      tbl_stats = pmap(
        list(
          .data$summary_type, .data$variable,
          .data$var_label, .data$stat_display, .data$df_stats
        ),
        function(summary_type, variable, var_label, stat_display, df_stats) {
          df_stats_to_tbl(
            data = data, variable = variable, summary_type = summary_type, by = by,
            var_label = var_label, stat_display = stat_display,
            df_stats = df_stats, missing = missing, missing_text = missing_text
          )
        }
      )
    ) %>%
    select(var_type = "summary_type", "var_label", "tbl_stats") %>%
    unnest("tbl_stats") %>%
    select("variable", "var_type", "var_label", everything())

  # table of column headers ----------------------------------------------------
  x <-
    .create_gtsummary_object(
      table_body = table_body,
      meta_data = meta_data,
      inputs = tbl_custom_summary_inputs,
      N = nrow(data),
      call_list = list(tbl_custom_summary = match.call()),
      by = by,
      df_by = df_by(data, by)
    )

  # adding "modify_stat_" information ------------------------------------------
  x$table_styling$header$modify_stat_N <- nrow(data)
  if (!is.null(by)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::left_join(
        x$df_by %>%
          select(
            column = "by_col",
            modify_stat_n = "n",
            modify_stat_p = "p",
            modify_stat_level = "by_chr"
          ),
        by = "column"
      )
  } else {
    x$table_styling$header <-
      x$table_styling$header %>%
      mutate(
        modify_stat_n = .data$modify_stat_N,
        modify_stat_p = .data$modify_stat_n / .data$modify_stat_N,
        modify_stat_level = ifelse(.data$column %in% "stat_0", translate_text("Overall"), NA_character_)
      )
  }


  # adding headers and footnote ------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = all_stat_cols(),
      footnote = footnote_stat_label(meta_data)
    ) %>%
    modify_header(
      label = paste0("**", translate_text("Characteristic"), "**"),
      all_stat_cols() ~
        ifelse(is.null(by),
          "**N = {style_number(N)}**",
          "**{level}**, N = {style_number(n)}"
        )
    )

  # assign class and return final tbl ------------------------------------------
  class(x) <- c("tbl_custom_summary", "tbl_summary", class(x))

  x
}


# generate metadata table --------------------------------------------------------------
# for survey objects pass the full survey object to `survey` argument, and `design$variables` to `data` argument
generate_metadata_custom_summary <- function(data, stat_fns, include,
                                             value, by, type, label,
                                             statistic, digits, survey = NULL) {
  # converting tidyselect formula lists to named lists -------------------------
  value <-
    .formula_list_to_named_list(
      x = value,
      data = data %>% select(any_of(include)), # adding here a select
      arg_name = "value"
    )

  # creating a table with meta data about each variable ------------------------
  meta_data <- tibble(
    variable = include,
    # assigning our best guess of the type, the final type is assigned below
    # we make a guess first, so users may use the gtsummary tidyselect functions for type
    summary_type = assign_summary_type(
      data = data %>% select(any_of(include)), # add here a select
      variable = .data$variable,
      summary_type = NULL, value = value
    )
  )
  # excluding by variable
  if (!is.null(by)) meta_data <- filter(meta_data, .data$variable != by)

  # updating type --------------------------------------------------------------
  # updating type of user supplied one
  # converting tidyselect formula lists to named lists
  type <- .formula_list_to_named_list(
    x = type,
    data = data,
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "type",
    type_check = chuck(type_check, "is_string", "fn"),
    type_check_msg = chuck(type_check, "is_string", "msg")
  )

  # updating meta data object with new types
  meta_data <-
    meta_data %>%
    mutate(
      summary_type = assign_summary_type(
        data = data, variable = .data$variable,
        summary_type = type, value = value, check_assignment = TRUE
      )
    )

  # converting tidyselect formula lists to named lists -------------------------
  stat_fns <-
    .formula_list_to_named_list(
      x = stat_fns,
      data = data %>% select(any_of(include)),
      var_info = meta_data_to_var_info(meta_data),
      arg_name = "stat_fns",
      type_check = chuck(type_check, "is_function_or_string", "fn"),
      type_check_msg = chuck(type_check, "is_function_or_string", "msg")
    )
  label <- .formula_list_to_named_list(
    x = label,
    data = data %>% select(any_of(include)), # adding a select
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "label",
    type_check = chuck(type_check, "is_character", "fn"),
    type_check_msg = chuck(type_check, "is_character", "msg")
  )
  statistic <- .formula_list_to_named_list(
    x = statistic,
    data = data %>% select(any_of(include)), # adding a select
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "statistic",
    type_check = chuck(type_check, "is_character", "fn"),
    type_check_msg = chuck(type_check, "is_character", "msg")
  )
  digits <- .formula_list_to_named_list(
    x = digits,
    data = data,
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "digits",
    type_check = chuck(type_check, "digits", "fn"),
    type_check_msg = chuck(type_check, "digits", "msg")
  )

  # assigning variable characteristics -----------------------------------------
  # if (is.null(survey)) {
  df_stats_function <- df_custom_stats_fun # custom functions
  data_for_df_stats <- data
  # } else {
  #   df_stats_function <- df_stats_fun_survey
  #   data_for_df_stats <- survey
  # }

  meta_data <-
    meta_data %>%
    mutate(
      dichotomous_value = assign_dichotomous_value(data, .data$variable, .data$summary_type, value),
      var_label = assign_var_label(data, .data$variable, label),
      stat_display = assign_stat_display(data, .data$variable, .data$summary_type, statistic),
      stat_label = stat_label_match(.data$stat_display),
      sort = assign_sort(.data$variable, .data$summary_type, sort = NULL), # alphanumeric only
      df_stats = pmap(
        list(
          .data$summary_type, .data$variable,
          .data$dichotomous_value,
          .data$stat_display, .data$var_label
        ),
        ~ df_stats_function(
          summary_type = ..1, variable = ..2,
          dichotomous_value = ..3,
          stat_fn = stat_fns[[..2]], # passing the appropriate stat_fn
          stat_display = ..4,
          data = data_for_df_stats, by = by,
          percent = percent, digits = digits,
          var_label = ..5
        )
      )
    )

  meta_data
}


# df_custom_stats_fun -----------------------------------------------------------------
# this function creates df_stats in the tbl_summary meta data table
# and includes the number of missing values
df_custom_stats_fun <- function(summary_type, variable, dichotomous_value, stat_fn,
                                stat_display, data, by, percent, digits, var_label) {
  # compute custom stats using dedicate summarize function
  t1 <- summarize_custom(
    data = data, stat_fn = stat_fn, variable = variable,
    by = by, stat_display = stat_display,
    summary_type = summary_type,
    dichotomous_value = dichotomous_value
  )

  # adding the N_obs and N_missing, etc
  t2 <- summarize_categorical(
    data = mutate_at(data, vars(all_of(variable)), is.na),
    variable = variable,
    by = by,
    dichotomous_value = TRUE,
    sort = "alphanumeric", percent = "column",
    stat_display = "{n}"
  ) %>%
    select(-"stat_display") %>%
    rename(p_miss = "p", N_obs = "N", N_miss = "n") %>%
    mutate(
      N_nonmiss = .data$N_obs - .data$N_miss,
      p_nonmiss = 1 - .data$p_miss
    )

  # returning table will all stats
  merge_vars <- switch(!is.null(by),
    c("by", "variable")
  ) %||% "variable"
  return <- left_join(t1, t2, by = merge_vars)

  # adding underlying column name
  if ("by" %in% names(return)) {
    return <-
      return %>%
      left_join(df_by(data, by)[c("by", "by_col")], by = "by") %>%
      rename(col_name = "by_col")
  } else {
    return$col_name <- "stat_0"
  }

  # adding label column
  if ("variable_levels" %in% names(return)) {
    return$label <- as.character(return$variable_levels)
  } else {
    return$label <- var_label
  }

  # adding formatting function as attr to summary statistics columns
  return <- adding_formatting_as_attr(
    df_stats = return, data = data, variable = variable,
    summary_type = summary_type, stat_display = stat_display, digits = digits
  )

  return
}

# summarize_custom -------------------------------------------------------------

summarize_custom <- function(data, stat_fn, variable, by, stat_display,
                             summary_type, dichotomous_value) {
  # prepping data set
  df_by <- df_by(data, by)

  if (!is.null(dichotomous_value)) {
    data[[variable]] <- forcats::fct_expand(
      as.factor(data[[variable]]),
      as.character(dichotomous_value)
    )
  }

  group_vars <- c(
    switch(!is.null(by),
      by
    ),
    switch(summary_type %in% c("categorical", "dichotomous"),
      variable
    )
  )
  full_data <- data # include missing and ungrouped

  data <- data %>%
    dplyr::filter(!is.na(.data[[variable]])) %>%
    dplyr::group_by(dplyr::across(all_of(group_vars)), .drop = FALSE)

  # calculating stats
  df_stats <- data %>%
    dplyr::group_modify(
      stat_fn,
      full_data = full_data,
      variable = variable,
      by = by,
      type = summary_type,
      stat_display = stat_display,
      .keep = TRUE
    ) %>%
    dplyr::rename(any_of(c(by = by))) %>%
    dplyr::mutate(variable = .env$variable) %>%
    dplyr::ungroup() %>%
    dplyr::rename(any_of(c(variable_levels = variable)))

  # replacing by variable with original (non-factor version)
  if (!is.null(by)) {
    df_stats <-
      df_stats %>%
      select(by_fct = "by", everything()) %>%
      left_join(df_by[c("by", "by_fct")], by = "by_fct") %>%
      select(-"by_fct")
  }

  # adding stat_display to the data frame
  if (summary_type == "continuous2") {
    return <-
      dplyr::cross_join(
        df_stats,
        tibble(
          variable_levels = map_chr(stat_display, ~ stat_label_match(.x) %>% unlist()),
          stat_display = stat_display
        )
      ) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  } else {
    return <-
      df_stats %>%
      mutate(stat_display = .env$stat_display) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  }

  # filtering if dichotomous value
  if (!is.null(dichotomous_value)) {
    return <- return %>%
      filter(.data$variable_levels == !!dichotomous_value) %>%
      select(-"variable_levels")
  }

  # returning final object
  return
}
