#' Italicize labels in gtsummary tables
#'
#' @param x an object created using gtsummary functions
#' @author Daniel Sjoberg
#' @export

tab_style_italicize_labels <- function(x) {
  # input checks ---------------------------------------------------------------
  if(!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvsummary")) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvsummary'")
  }

  # italicize labels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_italicize_labels"]] <- glue(
    "gt::tab_style(style = gt::cells_styles(text_style = 'italic'), ",
    "locations = gt::cells_data(columns = gt::vars(label),",
    "rows = row_type == 'label'))"
  )

  x
}
