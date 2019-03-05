#' Bold levels in gtsummary tables
#'
#' @param x an object created using gtsummary functions
#' @author Daniel Sjoberg
#' @export

tab_style_bold_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if(!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvsummary")) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvsummary'")
  }

  # bold levels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_bold_levels"]] <- glue(
    "gt::tab_style(style = gt::cells_styles(text_weight = 'bold'), ",
    "locations = gt::cells_data(columns = gt::vars(label),",
    "rows = row_type %in% c('level', 'missing')))"
  )

  x
}
