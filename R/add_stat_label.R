#' Add statistic labels
#'
#' Adds labels describing the summary statistics presented for
#' each variable in the [tbl_summary] table.
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' @param location location where statistic label will be included.
#' `"column"` (default) adds a column with the statistic label, and
#' `"row"` to add the statistic label to the variable label row.
#' @param label a list of formulas or a single formula updating the statistic
#' label, e.g. `label = all_categorical() ~ "No. (%)"`
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_stat_label_ex1 <-
#'   trial %>%
#'   dplyr::select(trt, age, grade, response) %>%
#'   tbl_summary(by = trt) %>%
#'   add_stat_label()
#'
#' # update stat label and put it on the variable label row
#' tbl_stat_label_ex2 <-
#'   trial %>%
#'   dplyr::select(trt, age, grade, response) %>%
#'   tbl_summary(by = trt) %>%
#'   add_stat_label(
#'     location = "row",
#'     label = list(all_categorical() ~ "no. (%)",
#'                  all_continuous() ~ "med. (iqr)")
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_stat_label_ex1.png}{options: width=60\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_stat_label_ex2.png}{options: width=60\%}}
#'
add_stat_label <- function(x, location = NULL, label = NULL) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_summary")) {
    stop("`x=` must be class `tbl_summary`", call. = FALSE)
  }

  # setting defaults -----------------------------------------------------------
  location <-
    location %||%
    get_theme_element("add_stat_label-arg:location") %>%
    match.arg(choices = c("column", "row"))

  # adding some meta data only needed for merging (i.e. the row_type)
  meta_data_stat_label <-
    x$meta_data %>%
    mutate(
      row_type = switch(
        location,
        "row" = "label",
        "column" = ifelse(.data$summary_type == "categorical", "level", "label")
      )
    ) %>%
    select(c("variable", "row_type", "stat_label"))

  # processing statistic labels ------------------------------------------------
  label <- tidyselect_to_list(x$inputs$data, label,
                              .meta_data = x$meta_data, arg_name = "label")

  df_label <-
    label %>%
    unlist() %>%
    tibble::enframe(name = "variable", value = "stat_label") %>%
    dplyr::mutate_all(as.character) %>%
    # inner join puts everything in the same order
    {dplyr::inner_join(
      meta_data_stat_label %>% select(.data$variable),
      .,
      by = "variable"
    )}

  # updating meta_data_stat_label with specified stat labels
  meta_data_stat_label[
    meta_data_stat_label$variable %in% df_label$variable,
    c("variable", "stat_label")
  ] <- df_label

  # merging in new labels to table_body
  x$table_body <-
    x$table_body %>%
    select(c("variable", "row_type", "label")) %>%
    left_join(meta_data_stat_label, by = c("variable", "row_type")) %>%
    left_join(x$table_body, c("variable", "row_type", "label"))

  #  updating the label, if stat label is on variable label row
  if (location == "row") {
    x$table_body <-
      x$table_body %>%
      mutate(
        label = ifelse(
          .data$row_type == "label",
          glue("{label}, {stat_label}"),
          .data$label
        )
      ) %>%
      select(-.data$stat_label)
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
      )

    # adding new column to table_header
    x$table_header <-
      tibble(column = names(x$table_body)) %>%
      left_join(x$table_header, by = "column") %>%
      table_header_fill_missing()

    # updating header
    x <- modify_header_internal(x, stat_label = "**Statistic**")
  }

  # removing stat label footnote
  x$table_header <-
    x$table_header %>%
    # removing statistics presented footnote
    mutate(
      footnote = ifelse(startsWith(.data$column, "stat_"),
                        NA_character_, .data$footnote)
    )

  # keeping track of all functions previously run
  x$call_list <- c(x$call_list, list(add_stat_label = match.call()))

  return(x)
}
