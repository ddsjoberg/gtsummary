#' Italicize levels in gtsummary tables
#'
#' @param x an object created using gtsummary functions
#' @author Daniel Sjoberg
#' @export

tab_style_italicize_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if(!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvregression")) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvregression'")
  }

  # italicize levels -----------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_italicize_levels"]] <- glue(
    "tab_style(style = cells_styles(text_style = 'italic'), ",
    "locations = cells_data(columns = vars(label),",
    "rows = row_type %in% c('level', 'missing')))"
  )

  x$call_list <- c(x$call_list, list(tab_style_italicize_levels = match.call()))

  x
}
