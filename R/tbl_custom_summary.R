#' Create a table of summary statistics using a custom summary function
#'
#' \lifecycle{experimental}
#'
#' The `tbl_custom_summary` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.
#' This function is similar to [tbl_summary()] but allows you to provide
#' a custom function in charge of computing the statistics (see Details).
#'
#' @inheritParams tbl_summary
#' @param stat_fns Formula or list of formulas specifying the function to be
#' used to compute the statistics (see below for details and examples).
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
#' helpers, and arguments as include, by, type, value, digits, missing and
#' missing_text.
#'
#' @section stat_fns argument:
#' The stat_fns argument specify the custom function(s) to be used for computing
#' the summary statistics. For example, `stat_fns = everything() ~ foo`.
#'
#' Each function may take the following arguments:
#' `foo(data, full_data, variable, by, type, ...)`
#'     - `data=` is the input data frame passed to `tbl_custom_summary()`,
#'       subsetted according to the level of `by` or `variable` if any,
#'       excluding `NA` values of the current `variable`
#'     - `full_data=` is the full input data frame passed to `tbl_custom_summary()`
#'     - `variable=` is a string indicating the variable to perform the
#'       calculation on
#'     - `by=` is a string indicating the by variable from `tbl_custom_summary=`,
#'       if present
#'     - `type=` is a string indicating the type of variable
#'       (continous, categorical, ...)
#'     - `stat_display=` a string indicating the statistic to display (for the
#'       `statistic` argument, for that variable)
#'
#' The user-defined does not need to utilize each of these inputs. It's
#' encouraged the user-defined function accept `...` as each of the arguments
#' *will* be passed to the function, even if not all inputs are utilized by
#' the user's function, e.g. `foo(data, ...)` (see examples).
#'
#' One or two additional columns are added to `data` and `full_data`:
#'     - ".variable", a copy of the current variable defined in `variable`
#'     - ".by", a copy of the by variable defined in `by`, if any
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
#' @export
#' @return A `tbl_summary` object
#' @family tbl_summary tools
#' @family tbl_custom_summary tools
#' @seealso See \href{http://www.danieldsjoberg.com/gtsummary/articles/gallery.html}{table gallery} for additional examples
#' @author Joseph Larmarange
#' @examples
#' # Example 1 ----------------------------------
#' my_stats <- function(data, ...) {
#'   marker_sum = sum(data$marker, na.rm = TRUE)
#'   mean_age = mean(data$age, na.rm = TRUE)
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
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_custom_summary_ex1.png}{options: width=31\%}}


tbl_custom_summary <- function(
                        data, by = NULL, label = NULL,
                        stat_fns, statistic,
                        digits = NULL, type = NULL, value = NULL,
                        missing = NULL, missing_text = NULL,
                        include = everything(),
                        overall_row = FALSE,
                        overall_row_last = FALSE,
                        overall_row_label = NULL
                        ) {
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
    if (is.null(overall_row_label))
      overall_row_label <- translate_text("Overall")
    data$.overall <- TRUE
    attr(data$.overall, "label") <- overall_row_label

    if (overall_row_last) include <- c(include, ".overall")
    else include <- c(".overall", include)
  }

  # setting defaults from gtsummary theme --------------------------------------
  # THEME ELEMENTS SHOULD BE ADDED FOR tbl_custom_summary
  label <- label %||% get_theme_element("tbl_summary-arg:label")
  digits <- digits %||% get_theme_element("tbl_summary-arg:digits")
  type <- type %||% get_theme_element("tbl_summary-arg:type")
  value <- value %||% get_theme_element("tbl_summary-arg:value")
  missing <-
    missing %||%
    get_theme_element("tbl_summary-arg:missing", default = "ifany")
  missing_text <-
    missing_text %||%
    get_theme_element("tbl_summary-arg:missing_text",
                      default = translate_text("Unknown"))

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing, choices = c("ifany", "always", "no"))

  # checking input data --------------------------------------------------------
  tbl_summary_data_checks(data)

  # deleting obs with missing by values ----------------------------------------
  # saving variable labels
  if (!is.null(by) && sum(is.na(data[[by]])) > 0) {
    message(glue(
      "{sum(is.na(data[[by]]))} observations missing `{by}` have been removed. ",
      "To include these observations, use `forcats::fct_explicit_na()` on `{by}` ",
      "column before passing to `tbl_summary()`."
    ))

    data <- filter(data, !is.na(.data[[by]]))
  }

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_custom_summary_inputs <- as.list(environment())

  # checking function inputs ---------------------------------------------------
  # verify if checks about stat_fns should be added at this stage
  tbl_summary_input_checks(
    data, by, label, type, value, statistic,
    digits, missing, missing_text, sort = NULL # sort to NULL
  )

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
    select(var_type = .data$summary_type, .data$var_label, .data$tbl_stats) %>%
    unnest(.data$tbl_stats) %>%
    select(.data$variable, .data$var_type, .data$var_label, everything())

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

  # adding stat footnote (unless there are continuous2 vars)
  if (!"continuous2" %in% meta_data$summary_type) {
    x <-
      modify_table_styling(
        x,
        columns = starts_with("stat_"),
        footnote = footnote_stat_label(meta_data)
      )
  }

  # returning all results in a list --------------------------------------------
  # assigning tbl_custom_summary and tbl_summary classes
  class(x) <- c("tbl_custom_summary", "tbl_summary", class(x)) # custom class

  # adding headers
  if (is.null(by)) {
    x <- modify_header(
      x,
      stat_0 = "**N = {style_number(N)}**",
      label = paste0("**", translate_text("Characteristic"), "**")
    )
  } else {
    x <- modify_header(
      x,
      update = list(
        all_stat_cols(FALSE) ~ "**{level}**, N = {style_number(n)}",
        label ~ paste0("**", translate_text("Characteristic"), "**")
      )
    )
  }

  # running any additional mods ------------------------------------------------
  # ADD THEME FOR Ctbl_custom_summary
  x <-
    get_theme_element("tbl_summary-fn:addnl-fn-to-run", default = identity) %>%
    do.call(list(x))

  # returning tbl_custom_summary table -----------------------------------------
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
    arg_name = "type"
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
      arg_name = "stat_fns"
    )
  label <- .formula_list_to_named_list(
    x = label,
    data = data %>% select(any_of(include)), # adding a select
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "label"
  )
  statistic <- .formula_list_to_named_list(
    x = statistic,
    data = data %>% select(any_of(include)), # adding a select
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "statistic"
  )
  digits <- .formula_list_to_named_list(
    x = digits,
    data = data,
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "digits"
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
    select(-.data$stat_display) %>%
    rename(p_miss = .data$p, N_obs = .data$N, N_miss = .data$n) %>%
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
      rename(col_name = .data$by_col)
  }
  else {
    return$col_name <- "stat_0"
  }

  # adding label column
  if ("variable_levels" %in% names(return)) {
    return$label <- as.character(return$variable_levels)
  }
  else {
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
  data$.variable <- data[[variable]]
  if (!is.null(by)) data$.by <- data[[by]]

  if (!is.null(dichotomous_value))
    data <- data %>%
      mutate(
        # adding dichotomous level (in case it is unobserved)
        .variable = forcats::fct_expand(
          as.factor(.data$.variable),
          as.character(dichotomous_value)
        )
      )

  group_vars <- c(
    switch (!is.null(by), ".by"),
    switch (summary_type %in% c("categorical", "dichotomous"), ".variable")
  )
  data <- data %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::filter(!is.na(.data$.variable))

  # calculating stats
  df_stats <- data %>%
    dplyr::group_modify(
      stat_fn,
      full_data = data,
      variable = variable,
      by = by,
      type = summary_type,
      stat_display = stat_display,
      .keep = TRUE
    ) %>%
    dplyr::mutate(variable = variable) %>%
    dplyr::ungroup() %>%
    dplyr::rename(any_of(c(by = ".by", variable_levels = ".variable")))

  # replacing by variable with original (non-factor version)
  if (!is.null(by)) {
    df_stats <-
      df_stats %>%
      select(by_fct = .data$by, everything()) %>%
      left_join(df_by[c("by", "by_fct")], by = "by_fct") %>%
      select(-.data$by_fct)
  }

  # adding stat_display to the data frame
  if (summary_type == "continuous2") {
    return <-
      left_join(
        df_stats,
        tibble(
          variable_levels = map_chr(stat_display, ~ stat_label_match(.x) %>% unlist()),
          stat_display = stat_display
        ),
        by = character()
      ) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  }
  else {
    return <-
      df_stats %>%
      mutate(stat_display = .env$stat_display) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  }

  # filtering if dichotomous value
  if (!is.null(dichotomous_value)) {
    return <- return %>%
      filter(.data$variable_levels == !!dichotomous_value) %>%
      select(-.data$variable_levels)
  }

  # returning final object
  return
}

#' @rdname add_overall
#' @export
add_overall.tbl_custom_summary <- function(x, last = FALSE, col_label = NULL) {
  updated_call_list <- c(x$call_list, list(add_overall = match.call()))
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    stop(
      "Cannot add Overall column when no 'by' variable in original tbl_custom_summary"
    )
  }

  x_copy <- x

  # removing 'by' variable from data
  # (so it won't show up in the overall tbl_summary)
  x_copy$inputs[["data"]] <- select(x$inputs[["data"]], -x[["by"]])
  x_copy$inputs$include <- x_copy$inputs$include %>% setdiff(x[["by"]])

  # replacing the function call by variable to NULL to get results overall
  x_copy$inputs[["by"]] <- NULL

  # if overall row, already included in data
  x_copy$inputs$overall_row <- FALSE

  # calculating stats overall, and adding header row
  tbl_overall <- do.call(tbl_custom_summary, x_copy$inputs)

  # merging overall results
  x <- add_overall_merge(x, tbl_overall, last, col_label)

  x$call_list <- updated_call_list
  x
}
