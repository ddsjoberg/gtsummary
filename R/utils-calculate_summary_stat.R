#' This function takes in the meta data table, and calls the appropriate summarize function.
#'
#' @param data Data frame
#' @param variable Character variable name in \code{data} that will be tabulated
#' @param by Character variable name in\code{data} that Summary statistics for
#' \code{variable} are stratified
#' @param summary_type A list that includes specified summary types.
#' @param var_label String label
#' @param dichotomous_value If the output is dichotomous, then this is the value
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{glue::glue()} inputs with n, N, and p as input options.
#' of the variable that will be displayed.
#' @param digits integer indicating the number of decimal places to be used.
#' @param class variable class.  If class is NA, then all values are NA, and no
#' summary statistics will be calculated.
#' @param missing whether to include NA values in the table. `missing` controls
#' if the table includes counts of NA values: the allowed values correspond to
#' never ("no"), only if the count is positive ("ifany") and even for
#' zero counts ("always"). Default is "ifany".
#' @keywords internal
#' @author Daniel Sjoberg

calculate_summary_stat <- function(data, variable, by, summary_type,
                                   dichotomous_value, var_label, stat_display,
                                   digits, class, missing) {

  # if class is NA, then do not calculate summary statistics
  if (is.na(class)) {
    # empty results table when no by variable
    if (is.null(by)) {
      return(
        tibble(
          row_type = c("label", "missing"),
          label = c(var_label, "Unknown"),
          stat_overall = c(NA_character_, as.character(nrow(data)))
        )
      )
    }
    # empty results table when there is a by variable
    if (!is.null(by)) {
      stat_col_names <- df_by(data, by)[["by_col"]]
      return(
        dplyr::data_frame(
          row_type = c("label", "missing"),
          label = c(var_label, "Unknown")
        ) %>%
          dplyr::left_join(
            table(data[[by]]) %>%
              as.matrix() %>%
              t() %>%
              as_tibble() %>%
              mutate_all(as.character) %>%
              set_names(stat_col_names) %>%
              mutate_(row_type = ~"missing")
          )
      )
    }
  }

  # return data table with continuous summary stats
  if (summary_type == "continuous") {
    return(
      summarize_continuous(
        data, variable, by, digits,
        var_label, stat_display, missing
      )
    )
  }

  # return data table with categorical or dichotomous summary stats
  if (summary_type %in% c("categorical", "dichotomous")) {
    return(
      summarize_categorical(
        data, variable, by, var_label,
        stat_display, dichotomous_value, missing
      )
    )
  }
}

# calculate_summary_stat(data = mtcars, variable = "hp", by = "am",
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001", class = NA)
# calculate_summary_stat(data = mtcars, variable = "hp", by = NULL,
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001", class = NA)
# calculate_summary_stat(data = mtcars, variable = "hp", by = "am",
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001")
#
# calculate_summary_stat(data = mtcars, variable = "hp", by = NULL,
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001")
#
#
# calculate_summary_stat(data = mtcars, variable = "cyl", by = NULL,
#                        summary_type = "categorical", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{n}/{N} ({p}%)",
#                        digits = NULL, pvalue = "<0.0001")
