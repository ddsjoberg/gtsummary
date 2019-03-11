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
#' the label doesn't exist, the variable name will be used.
#' @param type A list that includes specified summary types.  Accepted values
#' are `c("continuous", "categorical", "dichotomous")`,
#' e.g. `type = list(age = "continuous", female = "dichotomous")`.
#' If type not specified for a variable, the function
#' will default to an appropriate summary type.
#' @param statistic A list of the type of statistics to return.  The list can contain
#' two named elements (`continuous` and `categorical`). The default is
#' `list(continuous = "{median} ({p25}, {p75})", categorical = "{n} ({p}%)")`.
#' The syntax follows from the \code{\link[glue]{glue}} function.
#' For categorical variables the choices the following statistics are available to
#' report: `{n}` (frequency), `{N}` (denominator, or cohort size), `{p}` (percent).
#' For continuous variables, any quantile may be returned with `{p##}`, where ##
#' is any integer from 0 to 100. For example, `{p25}` and `{p75}` returns
#' the 25th and 75th quantiles.  The median (`{median}`), mean (`{mean}`),
#' standard deviation (`{sd}`), variance (`{var}`), minimum (`{min}`), and
#' maximum (`{max}`) are available.  In fact, any function that takes the form
#' `foo(x, na.rm = TRUE)` should work.
#' @param digits integer indicating the number of decimal places to round continuous
#' summary statistics. `sprintf(glue::glue("%.{digits}f"), x)`
#' @param group Character vector of an ID or grouping variable.  Summary statistics
#' will not be printed for this column. The column may be used in \code{\link{add_comparison}} to
#' calculate p-values with correlated data. Default is `NULL`
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @return List of summary statistics to be converted to a `gt` object
#' @author Daniel Sjoberg
#' @export
#' @examples
#' tbl_overall <- tbl_summary(trial)
#' tbl_trt <- tbl_summary(trial, by = "trt")
#' tbl_lbls <- mtcars %>% tbl_summary(label = list(cyl = "No. Cylinders"))

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
  if (!is.null(by)) meta_data <- meta_data %>% filter(!!parse_expr("variable != by"))
  # excluding id variable
  if (!is.null(group)) meta_data <- meta_data %>% filter(!!parse_expr("!variable %in% group"))

  # assigning variable characteristics
  meta_data <- meta_data %>%
    mutate(
      # assigning class, if entire var is NA, then assigning class NA
      class = assign_class(data, .data$variable),
      summary_type = assign_summary_type(
        data, .data$variable, .data$class, type
      ),
      dichotomous_value = assign_dichotomous_value(data, .data$variable, .data$summary_type, .data$class),
      var_label = assign_var_label(data, .data$variable, label),
      stat_display = assign_stat_display(.data$summary_type, statistic),
      stat_label = stat_label_match(.data$stat_display),
      digits = continuous_digits_guess(
        data, .data$variable, .data$summary_type, .data$class, digits
      )
    )

  # calculating summary statistics
  table_body <-
    meta_data %>%
    mutate(
      # creating summary stat table formatted properly
      stat_table = pmap(
        list(
          .data$variable, .data$summary_type, .data$dichotomous_value,
          .data$var_label, .data$stat_display, .data$digits, .data$class
        ),
        ~ calculate_summary_stat(
          data,
          variable = ..1, by = get("by"), summary_type = ..2,
          dichotomous_value = ..3, var_label = ..4, stat_display = ..5,
          digits = ..6, class = ..7, missing = missing
        )
      )
    ) %>%
    select(c("variable", "summary_type", "stat_table")) %>%
    unnest(!!sym("stat_table"))

  # returning all results in a list
  results <- list(
    gt_calls = eval(gt_tbl_summary),
    table_body = table_body %>% select(-c("summary_type")),
    meta_data = meta_data,
    inputs = tbl_summary_inputs,
    call_list = list(tbl_summary = match.call())
  )

  if (!is.null(by)) {
    results[["by"]] <- by
    results[["df_by"]] <- df_by(data, by)
  }

  # assigning a class of tbl_summary (for special printing in Rmarkdown)
  class(results) <- "tbl_summary"

  # adding headers
  if (is.null(by)) {
    results <- cols_label_summary(results, stat_overall = md("**N = {N}**"))
  } else {
    results <- cols_label_summary(results, stat_by = md("**{level}**, N = {n}"))
  }

  return(results)
}

# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_summary <- quote(list(
  # first call to the gt function
  gt = "gt(data = x$table_body)",

  # column headers
  cols_label_label = "cols_label(label = md('**Characteristic**'))",

  # label column indented and left just
  cols_align = glue(
    "cols_align(align = 'center') %>% ",
    "cols_align(align = 'left', columns = vars(label))"
  ),

  # do not print columns variable or row_type columns
  cols_hide = "cols_hide(columns = vars(variable, row_type))",

  # NAs do not show in table
  fmt_missing = "fmt_missing(columns = everything(), missing_text = '')",

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "tab_style(",
    "style = cells_styles(text_indent = px(10), text_align = 'left'),",
    "locations = cells_data(",
    "columns = vars(label),",
    "rows = row_type != 'label'",
    "))"
  ),

  # adding footnote listing statistics presented in table
  footnote_stat_label = glue(
    "tab_footnote(",
    "  footnote = '{footnote_stat_label(meta_data)}',",
    "  locations = cells_column_labels(",
    "    columns = vars(label))",
    ")"
  )
))
