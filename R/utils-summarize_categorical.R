#' Calculates and formats N's and percentages for categorical and dichotomous data
#'
#' @param data Data frame
#' @param variable Character variable name in `data` that will be tabulated
#' @param by Character variable name in `data` that Summary statistics for
#' `variable` are stratified
#' @param var_label String label
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{\link[glue]{glue}} inputs with n, N, and p as input options.
#' @param dichotomous_value If the output is dichotomous, then this is the value
#' of the variable that will be displayed.
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @return formatted summary statistics in a tibble.
#' @keywords internal
#' @author Daniel Sjoberg

summarize_categorical <- function(data, variable, by, var_label,
                                  stat_display, dichotomous_value, missing) {

  # counting total missing
  tot_n_miss <- sum(is.na(data[[variable]]))

  # tidyr::complete throws warning `has different attributes on LHS and RHS of join`
  # when variable has label.  So deleting it.
  attr(data[[variable]], "label") <- NULL
  if (!is.null(by)) attr(data[[by]], "label") <- NULL
  # same thing when the class "labelled" is included when labeled with the Hmisc package
  class(data[[variable]]) <- setdiff(class(data[[variable]]), "labelled")
  if (!is.null(by)) {
    class(data[[by]]) <- setdiff(class(data[[by]]), "labelled")
  }

  # grouping by var
  if (!is.null(by)) {
    data <-
      data %>%
      select(c(variable, by)) %>%
      set_names(c("variable", "by")) %>%
      left_join(df_by(data, by), by = "by") %>%
      select(c(variable, "by_col"))
  }
  else {
    data <-
      data %>%
      select(c(variable)) %>%
      set_names(c("variable")) %>%
      mutate_(by_col = ~"stat_0") %>%
      select(c(variable, "by_col"))
  }

  # nesting data and changing by variable
  tab <-
    data %>%
    stats::na.omit() %>%
    group_by_("by_col") %>%
    count_("variable") %>%
    complete_("variable", fill = list(n = 0)) %>%
    mutate_(
      N = ~sum(n),
      p = ~fmt_percent(n / N),
      stat = ~as.character(glue(stat_display))
    ) %>%
    select(c("by_col", "variable", "stat")) %>%
    spread_("by_col", "stat") %>%
    mutate_(
      row_type = ~"level",
      label = ~variable %>% as.character()
    ) %>%
    select(c("variable", "row_type", "label", starts_with("stat_")))

  # number of missing observations
  missing_count <-
    data %>%
    group_by_("by_col") %>%
    nest() %>%
    mutate_(
      missing_count = ~map_chr(data, ~.x[[1]] %>% is.na() %>% sum())
    ) %>%
    select(c("by_col", "missing_count")) %>%
    spread_("by_col", "missing_count") %>%
    mutate_(
      row_type = ~"missing",
      label = ~"Unknown"
    )


  # formatting for dichotomous variables
  if (!is.null(dichotomous_value)) {
    results <-
      tab %>%
      filter_("variable == dichotomous_value") %>%
      mutate_(
        row_type = ~"label",
        label = ~var_label
      ) %>%
      select(-c(variable)) %>%
      bind_rows(missing_count)
  }
  # formatting for categorical variables
  else {
    results <-
      tibble(
        row_type = "label",
        label = var_label
      ) %>%
      bind_rows(tab %>% select(-c("variable"))) %>%
      bind_rows(missing_count)
  }

  # excluding missing row if indicated
  if (missing == "no" | (missing == "ifany" & tot_n_miss == 0)) {
    results <-
      results %>%
      filter_("row_type != 'missing'")
  }

  results
}

# summarize_categorical(
#   data = lung, variable = "ph.karno", by = "sex", var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = 50, missing = "ifany"
# )
# summarize_categorical(
#   data = lung, variable = "ph.karno", by = "sex", var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = NULL
# )
# summarize_categorical(
#   data = lung, variable = "ph.karno", by = NULL, var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = 50
# )
#
# summarize_categorical(
#   data = mtcars, variable = "cyl", by = NULL, var_label = "WTF",
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL
# )
#
# summarize_categorical(
#   data = mtcars, variable = "cyl", by = "am", var_label = "WTF",
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL
# )
