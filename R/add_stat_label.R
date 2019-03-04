#' Adds a column showing a label for the summary statistics shown in each row
#'
#' Rather than simply printing the summary statistics, with the use of `add_stat_label()`,
#' a column labeling the summary statistics is added.
#'
#' @param x object with class `tbl_summary` from the \code{\link{tbl_summary}} function
#' @param iqr logical indicator whether '{p25}, {p75}' should
#' resolve to 'IQR'. Default is `TRUE`
#' @export
#' @examples
#' mtcars %>% tbl_summary() %>% add_stat_label()
#' mtcars %>% tbl_summary(by = "am") %>% add_stat_label(iqr = FALSE)
add_stat_label <- function(x, iqr = TRUE) {
  labels <-
    tibble::tribble(
      ~stat, ~label,
      "{min}", "Minimum",
      "{max}", "Maximum",
      "{median}", "Median",
      "{mean}", "Mean",
      "{sd}", "SD",
      "{var}", "Variance",
      "{n}", "n",
      "{N}", "N",
      "{p}%", "%",
      "{p}", "%"
    ) %>%
    bind_rows(
      tibble(stat = paste0("{p", 0:100, "}")) %>%
        mutate_(label = ~paste0(gsub("[^0-9\\.]", "", stat), "%"))
    )

  # adding IQR replacements if indicated
  if (iqr == TRUE) {
    labels <-
      bind_rows(
        tibble::tribble(
          ~stat, ~label,
          "{p25}, {p75}", "IQR"
        ),
        labels
      )
  }

  # now replacing each of the statistics with their labels
  x$meta_data$stat_label <- x$meta_data$stat_display
  for (i in 1:nrow(labels)) {
    x$meta_data$stat_label <-
      stringr::str_replace_all(
        x$meta_data$stat_label,
        stringr::fixed(labels$stat[i]),
        labels$label[i]
      )
  }

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
    "gt::cols_label(stat_label = gt::md('**Statistic**'))"


  return(x)
}
