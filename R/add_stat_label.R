#' Add statistic labels
#'
#' Adds labels describing the summary statistics presented for
#' each variable in the [tbl_summary] / [tbl_svysummary] table.
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' or with class `tbl_svysummary` from the [tbl_svysummary] function
#' @param location location where statistic label will be included.
#'  `"row"` (the default) to add the statistic label to the variable label row,
#'  and `"column"` adds a column with the statistic label.
#' @param label a list of formulas or a single formula updating the statistic
#' label, e.g. `label = all_categorical() ~ "No. (%)"`
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` or `tbl_svysummary` object
#' @examples
#' tbl <- trial %>%
#'   dplyr::select(trt, age, grade, response) %>%
#'   tbl_summary(by = trt)
#'
#' # Example 1 ----------------------------------
#' # Add statistic presented to the variable label row
#' add_stat_label_ex1 <-
#'   tbl %>%
#'   add_stat_label(
#'     # update default statistic label for continuous variables
#'     label = all_continuous() ~ "med. (iqr)"
#'   )
#'
#' # Example 2 ----------------------------------
#' add_stat_label_ex2 <-
#'   tbl %>%
#'   add_stat_label(
#'     # add a new column with statistic labels
#'     location = "column"
#'   )
#'
#' # Example 3 ----------------------------------
#' add_stat_label_ex3 <-
#'   trial %>%
#'   select(age, grade, trt) %>%
#'   tbl_summary(
#'     by = trt,
#'     type = all_continuous() ~ "continuous2",
#'     statistic = all_continuous() ~ c("{mean} ({sd})", "{min} - {max}"),
#'   ) %>%
#'   add_stat_label(label = age ~ c("Mean (SD)", "Min - Max"))
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_stat_label_ex1.png}{options: width=60\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_stat_label_ex2.png}{options: width=60\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{add_stat_label_ex3.png}{options: width=60\%}}

add_stat_label <- function(x, location = NULL, label = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!(inherits(x, "tbl_summary") | inherits(x, "tbl_svysummary"))) {
    stop("`x=` must be class `tbl_summary` or `tbl_svysummary`", call. = FALSE)
  }

  # setting defaults -----------------------------------------------------------
  location <- location %||%
    get_theme_element("add_stat_label-arg:location") %>%
    match.arg(choices = c("row", "column"))

  # no column stat label with continuous2 variables ----------------------------
  if (location == "column" && "continuous2" %in% x$meta_data$summary_type) {
    paste(
      "add_stat_label: Cannot combine `location = \"column\"` with multi-line summaries",
      "of continuous variables, e.g. summary types \"continuous2\".",
      "Updating argument value to `location = \"row\"`"
    ) %>%
      str_wrap() %>%
      inform()
    location <- "row"
  }

  # processing statistics label ------------------------------------------------
  # stat_label default
  stat_label <- as.list(x$meta_data$stat_label) %>% set_names(x$meta_data$variable)
  # converting input to named list
  if (!is_survey(x$inputs$data)) {
    label <-
      .formula_list_to_named_list(
        x = label,
        data = x$inputs$data[x$meta_data$variable],
        var_info = meta_data_to_var_info(x$meta_data),
        arg_name = "label"
      )
  } else {
    label <-
      .formula_list_to_named_list(
        x = label,
        data = x$inputs$data$variables[x$meta_data$variable],
        var_info = meta_data_to_var_info(x$meta_data),
        arg_name = "label"
      )
  }

  # updating the default values with values in label
  stat_label <- imap(stat_label, ~ label[[.y]] %||% .x)

  # adding some meta data needed for merging with table_body (i.e. the row_type)
  meta_data_stat_label <-
    x$meta_data %>%
    select(c("variable", "summary_type")) %>%
    left_join(
      imap_dfr(stat_label, ~ tibble(stat_label = .x, variable = .y)),
      by = "variable"
    ) %>%
    mutate(
      row_type = switch(
        location,
        "row" = ifelse(.data$summary_type == "continuous2", "level", "label"),
        "column" = ifelse(.data$summary_type == "categorical", "level", "label")
      )
    ) %>%
    select(c("variable", "row_type", "stat_label"))

  # merging in new labels to table_body
  meta_data_row_types <-
    select(meta_data_stat_label, .data$variable, .data$row_type) %>%
    distinct()

  x$table_body$stat_label <- NA_character_
  for (i in seq_len(nrow(meta_data_row_types))) {
    x$table_body$stat_label[
      x$table_body$variable == meta_data_row_types$variable[i] &
        x$table_body$row_type == meta_data_row_types$row_type[i]
    ] <-
      filter(meta_data_stat_label, .data$variable == meta_data_row_types$variable[i])$stat_label
  }

  #  updating the label, if stat label is on variable label row
  if (location == "row") {
    x$table_body <-
      x$table_body %>%
      left_join(x$meta_data %>% select(.data$variable, .data$summary_type),
        by = "variable"
      ) %>%
      mutate(
        label = case_when(
          .data$summary_type == "continuous2" & .data$row_type == "level" ~ stat_label,
          .data$summary_type != "continuous2" & .data$row_type == "label" ~ as.character(glue("{label}, {stat_label}")),
          TRUE ~ .data$label
        )
      ) %>%
      select(-.data$stat_label, -.data$summary_type)
  }
  # adding label for unknown if stat label column
  else if (location == "column") {
    x$table_body <-
      x$table_body %>%
      mutate(
        # adding in "n" for missing rows, and header
        stat_label = case_when(
          .data$row_type == "missing" ~ "n",
          TRUE ~ .data$stat_label
        )
      ) %>%
      select(any_of(c("variable", "row_type", "label", "stat_label")), everything())

    # adding new column to table_header
    x$table_header <-
      tibble(column = names(x$table_body)) %>%
      left_join(x$table_header, by = "column") %>%
      table_header_fill_missing()

    # updating header
    x <- modify_header(x, stat_label = paste0("**", translate_text("Statistic"), "**"))
  }

  # removing stat label footnote
  x$table_header <-
    x$table_header %>%
    # removing statistics presented footnote
    mutate(
      footnote = ifelse(startsWith(.data$column, "stat_"),
        NA_character_, .data$footnote
      )
    )

  # keeping track of all functions previously run
  x$call_list <- c(x$call_list, list(add_stat_label = match.call()))

  return(x)
}
