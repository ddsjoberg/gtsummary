#' Calculates and formats descriptive statistics for Table 1.
#'
#' The `fmt_table1` function calculates descriptive statistics by groups for
#' continuous, categorical, and dichotomous variables.  Review the
#' \href{https://github.mskcc.org/pages/datadojo/biostatR/articles/fmt_table1.html}{`fmt_table1` vignette}
#' for detailed examples.
#'
#' @param data data frame.
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
#' `median`, `q1` (first quartile), `q3` (third quartile), `mean`, `sd` (standard deviation),
#' `min` (minimum), `max` (maximum).  For categorical variables the choices are `n` (frequency),
#' `N` (denominator, or cohort size), `p` (percent).  The defaults are
#' `continuous = "{median} ({q1}, {q3})"` and `categorical = "{n} ({p}\%)"`.
#' The syntax follows from the \code{\link[glue]{glue}} function.  Dichotomous variables
#' follow the same format as categorical.
#' @param digits integer indicating the number of decimal places to round continuous
#' summary statistics. `sprintf(glue::glue("%.{digits}f"), x)`
#' @param id Character vector of an ID or grouping variable.  Summary statistics
#' will not be printed for this column. The column may be used in \code{\link{add_comparison}} to
#' calculate p-values with correlated data. Default is `NULL`
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @return Data frame including formatted descriptive statistics.
#' @examples
#' fmt_table1(trial, by = "trt")
#' 
#' # convert numeric 'am' to factor to display nicely in header
#' mtcars %>%
#'   dplyr::mutate(am = factor(am, c(0, 1), c("Automatic", "Manual"))) %>%
#'   fmt_table1(by = "am") %>%
#'   add_comparison()
#' @export

fmt_table1 <- function(data, by = NULL, label = NULL, type = NULL,
                       statistic = NULL, digits = NULL, id = NULL,
                       missing = c("ifany", "always", "no")) {
  missing <- match.arg(missing)
  # ungrouping data
  data <- data %>% dplyr::ungroup()

  # will return call, and all object passed to in fmt_table1 call
  # the object func_inputs is a list of every object passed to the function
  fmt_table1_inputs <- as.list(environment())

  # checking function inputs
  fmt_table1_input_checks(
    data, by, label, type,
    statistic, digits, missing, id
  )

  # creating a table with meta data about each variable
  meta_data <- dplyr::data_frame(.variable = names(data))
  # excluding by variable
  if (!is.null(by)) meta_data <- meta_data %>% dplyr::filter_(~ .variable != by)
  # excluding id variable
  if (!is.null(id)) meta_data <- meta_data %>% dplyr::filter_(~ !(.variable %in% id))

  # assigning variable characteristics
  meta_data <- meta_data %>%
    dplyr::mutate_(
      # assigning class, if entire var is NA, then assigning class NA
      .class = ~ assign_class(data, .variable),
      .summary_type = ~ assign_summary_type(
        data, .variable, .class, type
      ),
      .dichotomous_value = ~ assign_dichotomous_value(data, .variable, .summary_type, .class),
      .var_label = ~ assign_var_label(data, .variable, label),
      .stat_display = ~ assign_stat_display(.summary_type, statistic),
      .digits = ~ continuous_digits_guess(
        data, .variable, .summary_type, .class, digits
      )
    )

  # calculating summary statistics
  table1 <-
    meta_data %>%
    dplyr::mutate_(

      # creating summary stat table formatted properly
      .stat_table = ~ purrr::pmap(
        list(
          .variable, .summary_type, .dichotomous_value,
          .var_label, .stat_display, .digits, .class
        ),
        ~ calculate_summary_stat(data,
          variable = ..1, by = get("by"), summary_type = ..2,
          dichotomous_value = ..3, var_label = ..4, stat_display = ..5,
          digits = ..6, class = ..7, missing = missing
        )
      )
    ) %>%
    dplyr::select(dplyr::one_of(".variable", ".summary_type", ".stat_table")) %>%
    tidyr::unnest_(".stat_table")


  #  adding header rows to table1
  if (is.null(by)) {
    header_list <-
      create_header(
        data = data,
        label = c("Variable"),
        stat_overall = c("N = {N}")
      )
  }
  if (!is.null(by)) {
    header_list <-
      create_header(
        data = data,
        by = by,
        label = c("Variable", ""),
        stat_by = c("{level}", "N = {n}")
      )
  }

  # stacking header on top of table1
  table1 <-
    header_list %>%
    purrr::map_dfc(~.x) %>%
    dplyr::bind_rows(table1) %>%
    dplyr::select(dplyr::one_of(".variable", "row_type"), dplyr::everything())


  # assigning a class of fmt_table1 (for special printing in Rmarkdown)
  results <- list()
  class(results) <- "fmt_table1"

  # returning all results in a list
  results[["table1"]] <- table1 %>% dplyr::select(-dplyr::one_of(".summary_type"))
  results[["by"]] <- by
  results[["meta_data"]] <- meta_data
  results[["call"]] <- sys.call()
  results[["inputs"]] <- fmt_table1_inputs
  results[["call_list"]] <- list(fmt_table1 = match.call())

  return(results)
}
