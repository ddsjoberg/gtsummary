#' Calculates and formats summary statistics for continuous data
#'
#' @param data data frame
#' @param variable Character variable name in `data` that will be tabulated
#' @param by Character variable name in `data` that Summary statistics for
#' `variable` are stratified
#' @param digits integer indicating the number of decimal places to be used.
#' @param var_label string label
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{\link[glue]{glue}} inputs with n, N, and p as input options.
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @return formatted summary statistics in a tibble.
#' @keywords internal


summarize_continuous <- function(data, variable, by, digits,
                                 var_label, stat_display, missing) {

  # counting total missing
  tot_n_miss <- sum(is.na(data[[variable]]))

  # keeping the variable and by and renaming each
  data <-
    data %>%
    dplyr::select(dplyr::one_of(
      c(variable, by)
    )) %>%
    purrr::set_names(
      c(".variable", ".by")[1:length(c(variable, by))]
    )

  # grouping by variable (if applicable)
  if (!is.null(by)) {
    data <-
      data %>%
      dplyr::mutate_(
        .by = ~ paste0("stat_by", as.numeric(factor(.by)))
      ) %>%
      dplyr::group_by_(".by")
  }

  # calculating summary stats and number unknown
  results_long <-
    data %>%
    dplyr::summarise_(
      n_missing = ~ sum(is.na(.variable)),
      median = ~ median(.variable, na.rm = TRUE),
      q1 = ~ quantile(.variable, probs = 0.25, na.rm = TRUE),
      q3 = ~ quantile(.variable, probs = 0.75, na.rm = TRUE),
      min = ~ min(.variable, na.rm = TRUE),
      max = ~ max(.variable, na.rm = TRUE),
      mean = ~ mean(.variable, na.rm = TRUE),
      sd = ~ stats::sd(.variable, na.rm = TRUE),
      var = ~ sd^2,
      # pseudonyms
      med = ~median, q2 = ~median, p50 = ~median,
      p25 = ~q1, p75 = ~q3,
      minimum = ~min, maximum = ~max
    ) %>%
    dplyr::mutate_at(
      c(
        "median", "q1", "q3", "min", "max", "mean", "sd", "var",
        "med", "q2", "p50", "p25", "p75", "minimum", "maximum"
      ),
      function(x) sprintf(glue::glue("%.{digits}f"), x)
    ) %>%
    dplyr::mutate_(
      row_type = ~"label",
      label = ~var_label,
      stat = ~ as.character(glue::glue(stat_display))
    ) %>%
    dplyr::select(dplyr::one_of(c(dplyr::group_vars(data), "row_type", "label", "stat", "n_missing")))

  # appending missing N to bottom of data frame
  results_long_missing <-
    dplyr::bind_rows(
      results_long %>%
        dplyr::select(dplyr::one_of(c(dplyr::group_vars(data), "row_type", "label", "stat"))),
      results_long %>%
        dplyr::mutate_(
          row_type = ~"missing",
          label = ~"Unknown",
          stat = ~ as.character(n_missing)
        ) %>%
        dplyr::select(dplyr::one_of(c(dplyr::group_vars(data), "row_type", "label", "stat")))
    )

  # transposing to wide (by levels have their own columns)
  if (!is.null(by)) {
    results_wide <-
      results_long_missing %>%
      tidyr::spread_(".by", "stat")
  }
  # if no by var, just rename stat to stat_overall
  if (is.null(by)) {
    results_wide <-
      results_long_missing %>%
      purrr::set_names("row_type", "label", "stat_overall")
  }

  # excluding missing row if indicated
  if (missing == "no" | (missing == "ifany" & tot_n_miss == 0)) {
    results_wide <-
      results_wide %>%
      dplyr::filter_("row_type != 'missing'")
  }

  return(results_wide)
}
