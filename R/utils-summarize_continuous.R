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
#' @author Daniel Sjoberg
#' @importFrom stringr str_extract_all str_remove_all fixed

summarize_continuous <- function(data, variable, by, digits,
                                 var_label, stat_display, missing) {

  # counting total missing
  tot_n_miss <- sum(is.na(data[[variable]]))

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
  data <-
    data %>%
    group_by_("by_col") %>%
    nest(.key = "data")

  # nesting data and calculating descriptive stats
  stats <-
    data %>%
    mutate_(
      # extracting list of statisitcs that need to be calculated
      stat_name_list = ~str_extract_all(stat_display, "\\{.*?\\}") %>%
        map(str_remove_all, pattern = fixed("}")) %>%
        map(str_remove_all, pattern = fixed("{")),
      # calculating statistics
      stat_result_list = ~map2(
        data, stat_name_list,
        ~ calculate_single_stat(.x[[1]], .y)
      ),
      # converting stats into a tibble with names as the type of statistic (i.e. mean column is called mean)
      df_result = ~map2(
        stat_name_list, stat_result_list,
        ~ .y %>% t() %>% as_tibble() %>% set_names(.x)
      ),
      # rounding statistics and concatenating results
      stat = ~map_chr(
        df_result,
        ~.x %>%
          mutate_all(~sprintf(glue("%.{digits}f"), .)) %>%
          mutate_(
            stat = ~ as.character(glue(stat_display))
          ) %>%
          pull("stat")
      )
    ) %>%
    select(c("by_col", "stat")) %>%
    spread_("by_col", "stat") %>%
    mutate_(
      row_type = ~"label",
      label = ~var_label
    ) %>%
    select(c("row_type", "label", starts_with("stat_")))

  # number of missing observations
  missing_count <-
    data %>%
    mutate_(
      missing_count =
        ~map_chr(
          data,
          ~.x[[1]] %>% is.na() %>% sum()
        )
    ) %>%
    select(c("by_col", "missing_count")) %>%
    spread_("by_col", "missing_count") %>%
    mutate_(
      row_type = ~"missing",
      label = ~"Unknown"
    )

  # stacking stats and missing row
  result <-
    stats %>%
    bind_rows(missing_count)

  # excluding missing row if indicated
  if (missing == "no" | (missing == "ifany" & tot_n_miss == 0)) {
    result <-
      result %>%
      filter_("row_type != 'missing'")
  }

  result
}

# stat_name that are accepted
calculate_single_stat <- function(x, stat_name) {

    map_dbl(stat_name,
      function(name) {
        # calculating percentiles if requested
        if (name %in% paste0("p", 0:100)) {
          do.call(
            "quantile",
            list(
              x,
              probs = as.numeric(gsub("[^0-9\\.]", "", name))/100,
              na.rm = TRUE
            )
          )
         }
        # calculating summary stats, input MUST be a function name
        # first argument is x and must take argument 'na.rm = TRUE'
        else do.call(name, list(x, na.rm = TRUE))
      }
  )
}


# calculate_single_stat(mtcars$mpg, c("p50", "p70"))
#
# summarize_continuous(
#   data = mtcars, variable = "mpg", by = "vs", digits = 0,
#   var_label = "MPG!", stat_display = "{p30} ({p98})", missing = "no"
# )






