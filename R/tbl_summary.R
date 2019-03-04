#' Calculates and formats descriptive statistics for a data frame
#'
#' The `tbl_summary` function calculates descriptive statistics by groups for
#' continuous, categorical, and dichotomous variables.  Review the
#' \href{https://github.com/pages/ddsjoberg/gtsummary/articles/tbl_summary.html}{`tbl_summary` vignette}
#' for detailed examples.
#'
#' @param data a data frame
#' @param by a character name of a categorical variable in data, `by = "group"`.
#' Summary statistics will be calculated separately for each level of the by variable.
#' If `NULL`, summary statistics
#' are calculated using all observations.
#' @param label A list of variable labels,
#' e.g. `list(age = "Age, yrs", ptstage = "Path T Stage")`.  If `NULL`, the function
#' will take the label attribute (`attr(data$age, "label")`).  If
#' the label doesn't exist, then the label is assigned as the variable name.
#' @param type A list that includes specified summary types.  Accepted values
#' are `c("continuous", "categorical", "dichotomous")`,
#' e.g. `type = list(age = "continuous", female = "dichotomous")`.
#' If type not specified for a variable, the function
#' will default to an appropriate summary type.
#' @param statistic A list of the type of statistics to return.  The list can contain
#' two names lists (`continuous` and `categorical`).  The value within the list is the types of
#' summary statistics to be returned.  For continuous variables the choices are:
#' `median`, `p25` (first quartile), `p75` (third quartile), `mean`, `sd` (standard deviation),
#' `min` (minimum), `max` (maximum).  For categorical variables the choices are `n` (frequency),
#' `N` (denominator, or cohort size), `p` (percent).  The defaults are
#' `continuous = "{median} ({p25}, {p75})"` and `categorical = "{n} ({p}\%)"`.
#' The syntax follows from the \code{\link[glue]{glue}} function.  Dichotomous variables
#' follow the same format as categorical.
#' @param digits integer indicating the number of decimal places to round continuous
#' summary statistics. `sprintf(glue::glue("%.{digits}f"), x)`
#' @param group Character vector of an ID or grouping variable.  Summary statistics
#' will not be printed for this column. The column may be used in \code{\link{add_comparison}} to
#' calculate p-values with correlated data. Default is `NULL`
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @return List of summary statistics formattable to a `gt` object
#' @export
#' @author Daniel Sjoberg

tbl_summary <- function(data, by = NULL, label = NULL, type = NULL,
                        statistic = NULL, digits = NULL, group = NULL,
                        missing = c("ifany", "always", "no")) {
  missing <- match.arg(missing)
  # ungrouping data
  data <- data %>% ungroup()

  # will return call, and all object passed to in tbl_summary call
  # the object func_inputs is a list of every object passed to the function
  tbl_summary_inputs <- as.list(environment())

  # checking function inputs
  tbl_summary_input_checks(
    data, by, label, type,
    statistic, digits, missing, group
  )

  # creating a table with meta data about each variable
  meta_data <- tibble(variable = names(data))
  # excluding by variable
  if (!is.null(by)) meta_data <- meta_data %>% filter_(~ variable != by)
  # excluding id variable
  if (!is.null(group)) meta_data <- meta_data %>% filter_(~ !variable %in% group)

  # assigning variable characteristics
  meta_data <- meta_data %>%
    mutate_(
      # assigning class, if entire var is NA, then assigning class NA
      class = ~assign_class(data, variable),
      summary_type = ~assign_summary_type(
        data, variable, class, type
      ),
      dichotomous_value = ~assign_dichotomous_value(data, variable, summary_type, class),
      var_label = ~assign_var_label(data, variable, label),
      stat_display = ~assign_stat_display(summary_type, statistic),
      digits = ~continuous_digits_guess(
        data, variable, summary_type, class, digits
      )
    )

  # calculating summary statistics
  table_body <-
    meta_data %>%
    mutate_(
      # creating summary stat table formatted properly
      stat_table = ~pmap(
        list(
          variable, summary_type, dichotomous_value,
          var_label, stat_display, digits, class
        ),
        ~calculate_summary_stat(
          data,
          variable = ..1, by = get("by"), summary_type = ..2,
          dichotomous_value = ..3, var_label = ..4, stat_display = ..5,
          digits = ..6, class = ..7, missing = missing
        )
      )
    ) %>%
    select(c("variable", "summary_type", "stat_table")) %>%
    unnest_("stat_table")

  # assigning a class of tbl_summary (for special printing in Rmarkdown)
  results <- list()
  class(results) <- "tbl_summary"

  # returning all results in a list
  # first call to the gt function
  results[["gt_calls"]][["gt"]] <- "gt::gt(data = x$table_body)"
  # column headers
  results[["gt_calls"]][["cols_label:label"]] <-
    "gt::cols_label(label = gt::md('**Characteristic**'))"
  # label column indented and left just
  results[["gt_calls"]][["cols_align"]] <- glue(
    "gt::cols_align(align = 'center') %>% ",
    "gt::cols_align(align = 'left', columns = gt::vars(label))"
  )
  # do not print columns variable or row_type columns
  results[["gt_calls"]][["cols_hide"]] <-
    "gt::cols_hide(columns = gt::vars(variable, row_type))"
  # NAs do not show in table
  results[["gt_calls"]][["fmt_missing"]] <-
    "gt::fmt_missing(columns = gt::everything(), missing_text = '')"

  # indenting levels and missing rows
  results[["gt_calls"]][["tab_style:text_indent"]] <- glue(
    "gt::tab_style(",
    "style = gt::cells_styles(text_indent = gt::px(10), text_align = 'left'),",
    "locations = gt::cells_data(",
    "columns = gt::vars(label),",
    "rows = row_type != 'label'",
    "))"
  )

  results[["table_body"]] <- table_body %>% select(-c("summary_type"))
  if (!is.null(by)){
    results[["by"]] <- by
    results[["df_by"]] <- df_by(data, by)
  }
  results[["meta_data"]] <- meta_data
  results[["inputs"]] <- tbl_summary_inputs
  results[["call_list"]] <- list(tbl_summary = match.call())

  return(results)
}

