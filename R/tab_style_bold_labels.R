#' Bold labels in gtsummary tables
#'
#' @param x an object created using gtsummary functions
#' @author Daniel Sjoberg
#' @export

tab_style_bold_labels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvregression")) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvregression'")
  }

  # bold labels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_bold_labels"]] <- glue(
    "tab_style(style = cells_styles(text_weight = 'bold'), ",
    "locations = cells_data(columns = vars(label),",
    "rows = row_type == 'label'))"
  )

  x$call_list <- c(x$call_list, list(tab_style_bold_labels = match.call()))

  x
}
