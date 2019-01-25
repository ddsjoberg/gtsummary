#' Adds a column showing a label for the summary statistics shown in each row
#'
#' Rather than simply printing the summary statistics, with the use of `add_stat_label()`,
#' a column labeling the summary statistics is added.
#'
#' @param x object with class `fmt_table1` from the \code{\link{fmt_table1}} function
#' @param iqr logical indicator whether '{q1}, {q2}' and '{p25}, {p75}' should
#' resolve to 'IQR'. Default is `TRUE`
#' @export
#' @examples
#' mtcars %>% fmt_table1() %>% add_stat_label()
#' mtcars %>% fmt_table1(by = "am") %>% add_stat_label(iqr = FALSE)
add_stat_label <- function(x, iqr = TRUE) {
  labels <- tibble::tribble(
    ~stat, ~label,
    "{min}", "Minimum",
    "{minimum}", "Minimum",
    "{max}", "Maximum",
    "{maximum}", "Maximum",
    "{med}", "Median",
    "{median}", "Median",
    "{mean}", "Mean",
    "{p50}", "50%",
    "{q1}", "Q1",
    "{q2}", "Q2",
    "{q3}", "Q3",
    "{p25}", "25%",
    "{p75}", "Q3",
    "{sd}", "SD",
    "{var}", "Variance",
    "{n}", "n",
    "{N}", "N",
    "{p}%", "%",
    "{p}", "%"
  )

  # adding IQR replacements if indicated
  if (iqr == TRUE) {
    labels <-
      dplyr::bind_rows(
        tibble::tribble(
          ~stat, ~label,
          "{q1}, {q3}", "IQR",
          "{p25}, {p75}", "IQR"
        ),
        labels
      )
  }

  # now replacing each of the statistics with their labels
  x$meta_data$stat_label <- x$meta_data$.stat_display
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
    dplyr::mutate_(
      row_type = ~ ifelse(.summary_type == "categorical", "level", "label")
    ) %>%
    dplyr::select(dplyr::one_of(c(".variable", "row_type", "stat_label")))

  # merging in new labels to table1
  x$table1 <-
    x$table1 %>%
    dplyr::select(dplyr::one_of(c(".variable", "row_type", "label"))) %>%
    dplyr::left_join(meta_data_stat_label, by = c(".variable", "row_type")) %>%
    dplyr::left_join(x$table1, c(".variable", "row_type", "label"))

  x$table1 <-
    x$table1 %>%
    dplyr::mutate_(
      # adding in "n" for missing rows, and header
      stat_label = ~ dplyr::case_when(
        row_type == "missing" ~ "n",
        row_type == "header1" & is.null(x[["by"]]) ~ "Statistic",
        row_type == "header2" & !is.null(x[["by"]]) ~ "Statistic",
        TRUE ~ stat_label
      )
    )

  return(x)
}
