#' Adds a column showing a label for the summary statistics shown in each row
#'
#' Rather than simply printing the summary statistics, with the use of `add_stat_label()`,
#' a column labeling the summary statistics is added.
#'
#' @param x object with class `tbl_summary` from the \code{\link{tbl_summary}} function
#' @export
#' @examples
#' mtcars %>% tbl_summary() %>% add_stat_label()
add_stat_label <- function(x) {

  # adding some meta data only needed for merging (i.e. the row_type)
  meta_data_stat_label <-
    x$meta_data %>%
    mutate_(
      row_type = ~ ifelse(summary_type == "categorical", "level", "label")
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
    mutate_(
      # adding in "n" for missing rows, and header
      stat_label = ~ case_when(
        row_type == "missing" ~ "n",
        TRUE ~ stat_label
      )
    )

  # column headers
  x[["gt_calls"]][["cols_label:stat_label"]] <-
    "cols_label(stat_label = md('**Statistic**'))"

  # removing previous footnote about which statistics are presented
  x[["gt_calls"]][["footnote_stat_label"]] <- NULL

  return(x)
}
