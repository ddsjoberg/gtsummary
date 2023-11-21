#' Transform a **gt** table object to a raw HTML table
#'
#' Take a `gt_tbl` table object and transform it to an HTML table.
#'
#' @param data A gtsummary or gt table.
#'
#' @return A character object with an HTML table.
#'
#' @noRd
render_as_html <- function(data) {
  if (inherits(data, "gtsummary")) data <- as_gt(data)

  gt::as_raw_html(data) %>% gsub("id=\"[a-z]*?\"", "", .)
}
