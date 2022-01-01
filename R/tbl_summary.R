#' Create a table of summary statistics
#'
#' The `tbl_summary` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.  Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
#' for detailed examples.
#'
#' @param data A data frame
#' @param by A column name (quoted or unquoted) in `data`.
#' Summary statistics will be calculated separately for each level of the `by`
#' variable (e.g. `by = trt`). If `NULL`, summary statistics
#' are calculated using all observations. To stratify a table by two or more
#' variables, use `tbl_strata()`
#' @param label List of formulas specifying variables labels,
#' e.g. `list(age ~ "Age", stage ~ "Path T Stage")`.  If a
#' variable's label is not specified here, the label attribute
#' (`attr(data$age, "label")`) is used.  If
#' attribute label is `NULL`, the variable name will be used.
#' @param type List of formulas specifying variable types. Accepted values
#' are `c("continuous", "continuous2", "categorical", "dichotomous")`,
#' e.g. `type = list(age ~ "continuous", female ~ "dichotomous")`.
#' If type not specified for a variable, the function
#' will default to an appropriate summary type. See below for details.
#' @param value List of formulas specifying the value to display for dichotomous
#' variables.  See below for details.
#' @param statistic List of formulas specifying types of summary statistics to
#' display for each variable.  The default is
#' `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#' See below for details.
#' @param digits List of formulas specifying the number of decimal
#' places to round summary statistics. If not specified,
#' `tbl_summary` guesses an appropriate number of decimals to round statistics.
#' When multiple statistics are displayed for a single variable, supply a vector
#' rather than an integer.  For example, if the
#' statistic being calculated is `"{mean} ({sd})"` and you want the mean rounded
#' to 1 decimal place, and the SD to 2 use `digits = list(age ~ c(1, 2))`. User
#' may also pass a styling function: `digits = age ~ style_sigfig`
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
#' @param include variables to include in the summary table. Default is `everything()`
#'
#' @section select helpers:
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#select_helpers}{Select helpers}
#' from the \\{tidyselect\\} package and \\{gtsummary\\} package are available to
#' modify default behavior for groups of variables.
#' For example, by default continuous variables are reported with the median
#' and IQR.  To change all continuous variables to mean and standard deviation use
#' `statistic = list(all_continuous() ~ "{mean} ({sd})")`.
#'
#' All columns with class logical are displayed as dichotomous variables showing
#' the proportion of events that are `TRUE` on a single row. To show both rows
#' (i.e. a row for `TRUE` and a row for `FALSE`) use
#' `type = list(where(is.logical) ~ "categorical")`.
#'
#' The select helpers are available for use in any argument that accepts a list
#' of formulas (e.g. `statistic`, `type`, `digits`, `value`, `sort`, etc.)
#'
#' Read more on the [syntax] used through the package.
#'
#' @section type argument:
#' The `tbl_summary()` function has four summary types:
#'    - `"continuous"` summaries are shown on a *single row*. Most numeric
#'    variables default to summary type continuous.
#'    - `"continuous2"` summaries are shown on *2 or more rows*
#'    - `"categorical"` *multi-line* summaries of nominal data. Character variables,
#'    factor variables, and numeric variables with fewer than 10 unique levels default to
#'    type categorical. To change a numeric variable to continuous that
#'    defaulted to categorical, use `type = list(varname ~ "continuous")`
#'    - `"dichotomous"` categorical variables that are displayed on a *single row*,
#'    rather than one row per level of the variable.
#'    Variables coded as `TRUE`/`FALSE`, `0`/`1`, or `yes`/`no` are assumed to be dichotomous,
#'    and the `TRUE`, `1`, and `yes` rows are displayed.
#'    Otherwise, the value to display must be specified in the `value`
#'    argument, e.g. `value = list(varname ~ "level to show")`
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
#'   \item `{sum}` sum
#'   \item `{p##}` any integer percentile, where `##` is an integer from 0 to 100
#'   \item `{foo}` any function of the form `foo(x)` is accepted where `x` is a numeric vector
#' }
#' When the summary type is `"continuous2"`, pass a vector of statistics. Each element
#' of the vector will result in a separate row in the summary table.
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
#' }
#'
#' Note that for categorical variables, `{N_obs}`, `{N_miss}` and `{N_nonmiss}` refer
#' to the total number, number missing and number non missing observations
#' in the denominator, not at each level of the categorical variable.
#'
#' @export
#' @return A `tbl_summary` object
#' @family tbl_summary tools
#' @seealso See \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette} for detailed tutorial
#' @seealso See \href{http://www.danieldsjoberg.com/gtsummary/articles/gallery.html}{table gallery} for additional examples
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @author Daniel D. Sjoberg
#' @examples
#' # Example 1 ----------------------------------
#' tbl_summary_ex1 <-
#'   trial %>%
#'   select(age, grade, response) %>%
#'   tbl_summary()
#'
#' # Example 2 ----------------------------------
#' tbl_summary_ex2 <-
#'   trial %>%
#'   select(age, grade, response, trt) %>%
#'   tbl_summary(
#'     by = trt,
#'     label = list(age ~ "Patient Age"),
#'     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'     digits = list(age ~ c(0, 1))
#'   )
#'
#' # Example 3 ----------------------------------
#' # for convenience, you can also pass named lists to any arguments
#' # that accept formulas (e.g label, digits, etc.)
#' tbl_summary_ex3 <-
#'   trial %>%
#'   select(age, trt) %>%
#'   tbl_summary(
#'     by = trt,
#'     label = list(age = "Patient Age")
#'   )
#'
#' # Example 4 ----------------------------------
#' # multi-line summaries of continuous data with type 'continuous2'
#' tbl_summary_ex4 <-
#'   trial %>%
#'   select(age, marker) %>%
#'   tbl_summary(
#'     type = all_continuous() ~ "continuous2",
#'     statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
#'     missing = "no"
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
#'
#' \if{html}{Example 4}
#'
#' \if{html}{\figure{tbl_summary_ex4.png}{options: width=31\%}}

tbl_summary <- function(data, by = NULL, label = NULL, statistic = NULL,
                        digits = NULL, type = NULL, value = NULL,
                        missing = NULL, missing_text = NULL, sort = NULL,
                        percent = NULL, include = everything()) {
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

  # setting defaults from gtsummary theme --------------------------------------
  label <- label %||% get_theme_element("tbl_summary-arg:label")
  statistic <- statistic %||% get_theme_element("tbl_summary-arg:statistic")
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
  sort <- sort %||% get_theme_element("tbl_summary-arg:sort")
  percent <- percent %||% get_theme_element("tbl_summary-arg:percent",
    default = "column"
  )

  # matching arguments ---------------------------------------------------------
  missing <- match.arg(missing, choices = c("ifany", "always", "no"))
  percent <- match.arg(percent, choices = c("column", "row", "cell"))

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
  tbl_summary_inputs <- as.list(environment())

  # checking function inputs ---------------------------------------------------
  tbl_summary_input_checks(
    data, by, label, type, value, statistic,
    digits, missing, missing_text, sort
  )

  # generate meta_data --------------------------------------------------------
  meta_data <- generate_metadata(
    data = data %>% select(all_of(include)),
    value = value, by = by,
    type = type, label = label, statistic = statistic,
    digits = digits, percent = percent, sort = sort
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
      inputs = tbl_summary_inputs,
      N = nrow(data),
      call_list = list(tbl_summary = match.call()),
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
  # assigning a class of tbl_summary (for special printing in Rmarkdown)
  class(x) <- c("tbl_summary", class(x))

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
  x <-
    get_theme_element("tbl_summary-fn:addnl-fn-to-run", default = identity) %>%
    do.call(list(x))

  # returning tbl_summary table ------------------------------------------------
  x
}


# generate metadata table --------------------------------------------------------------
# for survey objects pass the full survey object to `survey` argument, and `design$variables` to `data` argument
generate_metadata <- function(data, value, by, type, label,
                              statistic, digits, percent, sort, survey = NULL) {
  # converting tidyselect formula lists to named lists -------------------------
  value <- .formula_list_to_named_list(x = value, data = data, arg_name = "value")

  # creating a table with meta data about each variable ------------------------
  meta_data <- tibble(
    variable = names(data),
    # assigning our best guess of the type, the final type is assigned below
    # we make a guess first, so users may use the gtsummary tidyselect functions for type
    summary_type = assign_summary_type(
      data = data, variable = .data$variable,
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
    type_check = is_string
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
  label <- .formula_list_to_named_list(
    x = label,
    data = data,
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "label",
    type_check = is.character
  )
  statistic <- .formula_list_to_named_list(
    x = statistic,
    data = data,
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "statistic",
    type_check = is_character
  )
  digits <- .formula_list_to_named_list(
    x = digits,
    data = data,
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "digits",
    type_check = function(x) rlang::is_integerish(x) || is_function(x) || purrr::every(x, ~rlang::is_integerish(.x) || is_function(.x))
  )
  sort <- .formula_list_to_named_list(
    x = sort,
    data = data,
    var_info = meta_data_to_var_info(meta_data),
    arg_name = "sort",
    type_check = is_string
  )

  # assigning variable characteristics -----------------------------------------
  if (is.null(survey)) {
    df_stats_function <- df_stats_fun
    data_for_df_stats <- data
  } else {
    df_stats_function <- df_stats_fun_survey
    data_for_df_stats <- survey
  }

  meta_data <-
    meta_data %>%
    mutate(
      dichotomous_value = assign_dichotomous_value(data, .data$variable, .data$summary_type, value),
      var_label = assign_var_label(data, .data$variable, label),
      stat_display = assign_stat_display(data, .data$variable, .data$summary_type, statistic),
      stat_label = stat_label_match(.data$stat_display),
      sort = assign_sort(.data$variable, .data$summary_type, sort),
      df_stats = pmap(
        list(
          .data$summary_type, .data$variable,
          .data$dichotomous_value,
          .data$sort, .data$stat_display, .data$var_label
        ),
        ~ df_stats_function(
          summary_type = ..1, variable = ..2,
          dichotomous_value = ..3,
          sort = ..4, stat_display = ..5,
          data = data_for_df_stats, by = by,
          percent = percent, digits = digits,
          var_label = ..6
        )
      )
    )

  meta_data
}
