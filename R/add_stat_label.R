#' Add statistic labels column
#'
#' Adds a column with labels describing the summary statistics presented for
#' each variable in the [tbl_summary] table.
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_stat_ex <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_summary() %>%
#'   add_stat_label()
#' @section Example Output:
#' \if{html}{\figure{tbl_stat_ex.png}{options: width=50\%}}
#'
add_stat_label <- function(x) {

  # adding some meta data only needed for merging (i.e. the row_type)
  meta_data_stat_label <-
    x$meta_data %>%
    mutate(
      row_type = ifelse(.data$summary_type == "categorical", "level", "label")
    ) %>%
    select(c("variable", "row_type", "stat_label"))

  # merging in new labels to table_body
  x$table_body <-
    x$table_body %>%
    select(c("variable", "row_type", "label")) %>%
    left_join(meta_data_stat_label, by = c("variable", "row_type")) %>%
    left_join(x$table_body, c("variable", "row_type", "label"))

  x$table_body <-
    x$table_body %>%
    mutate(
      # adding in "n" for missing rows, and header
      stat_label = case_when(
        .data$row_type == "missing" ~ "n",
        TRUE ~ .data$stat_label
      )
    )

  x$table_header <-
    tibble(column = names(x$table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing() %>%
    # removing statistics presented footnote
    mutate(
      footnote = ifelse(startsWith(.data$column, "stat_"),
                        NA_character_, .data$footnote)
    )

  # updating header
  x <- modify_header_internal(x, stat_label = "**Statistic**")

  # keeping track of all functions previously run
  x$call_list <- c(x$call_list, list(add_stat_label = match.call()))


  return(x)
}
