#' Create a table of summary statistics
#'
#' The `tbl_summary` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.  Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
#' for detailed examples.
#'
#' @param data A data frame
#' @param by A column name (quoted or unquoted) in `data`.
#' Summary statistics will be calculated separately for each level of the `by`
#' variable (e.g. `by = trt`). If `NULL`, summary statistics
#' are calculated using all observations.
#' @param label List of formulas specifying variables labels,
#' e.g. `list(age ~ "Age, yrs", stage ~ "Path T Stage")`.  If a
#' variable's label is not specified here, the label attribute
#' (`attr(data$age, "label")`) is used.  If
#' attribute label is `NULL`, the variable name will be used.
#' @param type List of formulas specifying variable types. Accepted values
#' are `c("continuous", "categorical", "dichotomous")`,
#' e.g. `type = list(starts_with(age) ~ "continuous", female ~ "dichotomous")`.
#' If type not specified for a variable, the function
#' will default to an appropriate summary type.  See below for details.
#' @param value List of formulas specifying the value to display for dichotomous
#' variables.  See below for details.
#' @param statistic List of formulas specifying types of summary statistics to
#' display for each variable.  The default is
#' `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#' See below for details.
#' @param digits List of formulas specifying the number of decimal
#' places to round continuous summary statistics. If not specified,
#' `tbl_summary` guesses an appropriate number of decimals to round statistics.
#' When multiple statistics are displayed for a single variable, supply a vector
#' rather than an integer.  For example, if the
#' statistic being calculated is `"{mean} ({sd})"` and you want the mean rounded
#' to 1 decimal place, and the SD to 2 use `digits = list(age ~ c(1, 2))`.
#' @param missing Indicates whether to include counts of `NA` values in the table.
#' Allowed values are `"no"` (never display NA values),
#' `"ifany"` (only display if any NA values), and `"always"`
#' (includes NA count row for all variables). Default is `"ifany"`.
#' @param missing_text String to display for count of missing observations.
#' Default is `"Unknown"`.
#' @param sort List of formulas specifying the type of sorting to perform for
#' categorical data. Options are `frequency` where results are sorted in
#' descending order of frequency and `alphanumeric`,
#' e.g. `sort = list(everything() ~ "frequency")`
#' @param percent Indicates the type of percentage to return. Must be one of
#' `"column"`, `"row"`, or `"cell"`. Default is `"column"`.
#' @param group DEPRECATED. Migrated to [add_p]
#'
#' @section select helpers:
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#select_helpers}{Select helpers}
#' from the \\{tidyselect\\} package and \\{gtsummary\\} package are available to
#' modify default behavior for groups of variables.
#' For example, by default continuous variables are reported with the median
#' and IQR.  To change all continuous variables to mean and standard deviation use
#' `statistic = list(all_continuous() ~ "{mean} ({sd})")`.
#'
#' All columns with class logical are displayed as dichotomous variables showing
#' the proportion of events that are `TRUE` on a single row. To show both rows
#' (i.e. a row for `TRUE` and a row for `FALSE`) use
#' `type = list(all_logical() ~ "categorical")`.
#'
#' The select helpers are available for use in any argument that accepts a list
#' of formulas (e.g. `statistic`, `type`, `digits`, `value`, `sort`, etc.)
#'
#' @section statistic argument:
#' The statistic argument specifies the statistics presented in the table. The
#' input is a list of formulas that specify the statistics to report. For example,
#' `statistic = list(age ~ "{mean} ({sd})")` would report the mean and
#' standard deviation for age; `statistic = list(all_continuous() ~ "{mean} ({sd})")`
#' would report the mean and standard deviation for all continuous variables.
#'  A statistic name that appears between curly brackets
#' will be replaced with the numeric statistic (see [glue::glue]).
#'
#' For categorical variables the following statistics are available to display.
#' \itemize{
#'   \item `{n}` frequency
#'   \item `{N}` denominator, or cohort size
#'   \item `{p}` formatted percentage
#' }
#' For continuous variables the following statistics are available to display.
#' \itemize{
#'   \item `{median}` median
#'   \item `{mean}` mean
#'   \item `{sd}` standard deviation
#'   \item `{var}` variance
#'   \item `{min}` minimum
#'   \item `{max}` maximum
#'   \item `{p##}` any integer percentile, where `##` is an integer from 0 to 100
#'   \item `{foo}` any function of the form `foo(x)` is accepted where `x` is a numeric vector
#' }
#'
#' @section type argument:
#' tbl_summary displays summary statistics for three types of data:
#' continuous, categorical, and dichotomous. If the type is not specified,
#' tbl_summary will do its best to guess the type.  Dichotomous variables
#' are categorical variables that are displayed on a single row in the
#' output table, rather than one row per level of the variable.
#' Variables coded as TRUE/FALSE, 0/1, or yes/no are assumed to be dichotomous,
#' and the TRUE, 1, and yes rows are displayed.
#' Otherwise, the value to display must be specified in
#' the `value` argument, e.g. `value = list(varname ~ "level to show")`
#' @export
#' @return A `tbl_summary` object
#' @family tbl_summary tools
#' @seealso See tbl_summary \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for detailed examples
#' @author Daniel D. Sjoberg
#' @examples
#' tbl_summary_ex1 <-
#'   trial[c("age", "grade", "response")] %>%
#'   tbl_summary()
#'
#' tbl_summary_ex2 <-
#'   trial[c("age", "grade", "response", "trt")] %>%
#'   tbl_summary(
#'     by = trt,
#'     label = list(age ~ "Patient Age"),
#'     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'     digits = list(age ~ c(0, 1))
#'   )
#'
#' # for convenience, you can also pass named lists to any arguments
#' # that accept formulas (e.g label, digits, etc.)
#' tbl_summary_ex3 <-
#'   trial[c("age", "trt")] %>%
#'   tbl_summary(
#'     by = trt,
#'     label = list(age = "Patient Age")
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_summary_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_summary_ex2.png}{options: width=45\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{tbl_summary_ex3.png}{options: width=45\%}}

tbl_summary <- function(data, by = NULL, label = NULL, statistic = NULL,
                        digits = NULL, type = NULL, value = NULL,
                        missing = c("ifany", "always", "no"),
                        missing_text = "Unknown", sort = NULL,
                        percent = c("column", "row", "cell"), group = NULL) {

  # converting bare arguments to string ----------------------------------------
  by <- var_input_to_string(data = data, select_input = !!rlang::enquo(by),
                            arg_name = "by", select_single = TRUE)

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing)
  percent <- match.arg(percent)

  # ungrouping data ------------------------------------------------------------
  tbl_summary_data_checks(data)
  data <- data %>% ungroup()

  # deleting obs with missing by values ----------------------------------------
  # saving variable labels
  if (!is.null(by) && sum(is.na(data[[by]])) > 0) {
    message(glue(
      "{sum(is.na(data[[by]]))} observations missing `{by}` have been removed. ",
      "To include these observations, use `forcats::fct_explicit_na()` on `{by}` ",
      "column before passing to `tbl_summary()`."
    ))
    lbls <- purrr::map(data, ~ attr(.x, "label"))
    data <- data[!is.na(data[[by]]), ]

    # re-applying labels---I think this will NOT be necessary after dplyr 0.9.0
    for (i in names(lbls)) {
      attr(data[[i]], "label") <- lbls[[i]]
    }
  }

  # deprecation note about group -----------------------------------------------
  if (!rlang::quo_is_null(rlang::enquo(group))) {
    lifecycle::deprecate_stop(
      "1.2.0",
      "gtsummary::tbl_summary(group=)",
      "add_p(group=)"
    )
  }

  # will return call, and all object passed to in tbl_summary call -------------
  # the object func_inputs is a list of every object passed to the function
  tbl_summary_inputs <- as.list(environment())

  # removing variables with unsupported variable types from data ---------------
  classes_expected <- c("character", "factor", "numeric", "logical", "integer", "difftime")
  var_to_remove <-
    map_lgl(data, ~ class(.x) %in% classes_expected %>% any()) %>%
    discard(. == TRUE) %>%
    names()
  data <- dplyr::select(data, -var_to_remove)
  if (length(var_to_remove) > 0) {
    message(glue(
      "Column(s) {glue_collapse(paste(sQuote(var_to_remove)), sep = ', ', last = ', and ')} ",
      "omitted from output.\n",
      "Accepted classes are {glue_collapse(paste(sQuote(classes_expected)), sep = ', ', last = ', or ')}."
    ))
  }

  # checking function inputs ---------------------------------------------------
  tbl_summary_input_checks(
    data, by, label, type, value, statistic,
    digits, missing, missing_text, sort
  )

  # converting tidyselect formula lists to named lists -------------------------
  value <- tidyselect_to_list(data, value, arg_name = "value")

  # creating a table with meta data about each variable ------------------------
  meta_data <- tibble(
    variable = names(data),
    # assigning class, if entire var is NA, then assigning class NA
    class = assign_class(data, .data$variable, classes_expected),
    # assigning our best guess of the type, the final type is assigned below
    # we make a guess first, so users may use the gtsummary tidyselect functions for type
    summary_type = assign_summary_type(
      data = data, variable = .data$variable, class = .data$class,
      summary_type = NULL, value = value
    )
  )
  # excluding by variable
  if (!is.null(by)) meta_data <- filter(meta_data, .data$variable != by)

  # updating type --------------------------------------------------------------
  # updating type of user supplied one
  if (!is.null(type)) {
    # converting tidyselect formula lists to named lists
    type <- tidyselect_to_list(data, type, .meta_data = meta_data, arg_name = "type")

    # updating meta data object with new types
    meta_data <-
      meta_data %>%
      mutate(
        summary_type = assign_summary_type(
          data = data, variable = .data$variable, class = .data$class,
          summary_type = type, value = value
        )
      )
  }

  # converting tidyselect formula lists to named lists -------------------------
  label <- tidyselect_to_list(data, label, .meta_data = meta_data, arg_name = "label")
  statistic <- tidyselect_to_list(data, statistic, .meta_data = meta_data, arg_name = "statistic")
  digits <- tidyselect_to_list(data, digits, .meta_data = meta_data, arg_name = "digits")
  sort <- tidyselect_to_list(data, sort, .meta_data = meta_data)

  # assigning variable characteristics -----------------------------------------
  meta_data <-
    meta_data %>%
    mutate(
      dichotomous_value = assign_dichotomous_value(data, .data$variable, .data$summary_type, .data$class, value),
      var_label = assign_var_label(data, .data$variable, label),
      stat_display = assign_stat_display(.data$variable, .data$summary_type, statistic),
      stat_label = stat_label_match(.data$stat_display),
      digits = continuous_digits_guess(
        data, .data$variable, .data$summary_type, .data$class, digits
      ),
      sort = assign_sort(.data$variable, .data$summary_type, sort),
      df_stats = pmap(
        list(.data$summary_type, .data$variable, .data$class, .data$dichotomous_value,
             .data$sort, .data$stat_display, .data$digits),
        function(summary_type, variable, class, dichotomous_value, sort, stat_display, digits) {
          switch(
            summary_type,
            "continuous" = summarize_continuous(data = data, variable = variable,
                                                 by = by, stat_display = stat_display,
                                                 digits = digits),
            "categorical" = summarize_categorical(data = data, variable = variable,
                                                   by = by, class = class,
                                                   dichotomous_value = dichotomous_value,
                                                   sort = sort, percent = percent),
            "dichotomous" = summarize_categorical(data = data, variable = variable,
                                                   by = by, class = class,
                                                   dichotomous_value = dichotomous_value,
                                                   sort = sort, percent = percent)
          )
        }
      )
    )

  # calculating summary statistics ---------------------------------------------
  table_body <-
    meta_data %>%
    mutate(
      tbl_stats = pmap(
        list(.data$summary_type, .data$variable,
             .data$var_label, .data$stat_display, .data$df_stats),
        function(summary_type, variable, var_label, stat_display, df_stats) {
          df_stats_to_tbl(
            data = data, variable = variable, summary_type = summary_type, by = by,
            var_label = var_label, stat_display = stat_display,
            df_stats = df_stats, missing = missing, missing_text = missing_text
          )
        }
      )
    ) %>%
    pull(.data$tbl_stats) %>%
    purrr::reduce(bind_rows)

  # table of column headers ----------------------------------------------------
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing() %>%
    mutate(
      footnote = ifelse(startsWith(.data$column, "stat_"),
                        footnote_stat_label(meta_data),
                        .data$footnote)
    )

  # returning all results in a list --------------------------------------------
  results <- list(
    table_body = table_body,
    table_header = table_header,
    meta_data = meta_data,
    inputs = tbl_summary_inputs,
    N = nrow(data),
    call_list = list(tbl_summary = match.call())
  )
  results$by <- by
  results$df_by <- df_by(data, by)

  # assigning a class of tbl_summary (for special printing in Rmarkdown)
  class(results) <- c("tbl_summary", "gtsummary")

  # adding headers
  if (is.null(by)) {
    results <- modify_header_internal(results,
                                      stat_0 = "**N = {N}**",
                                      label = "**Characteristic**")
  } else {
    results <- modify_header_internal(results,
                                      stat_by = "**{level}**, N = {n}",
                                      label = "**Characteristic**")
  }

  return(results)
}
