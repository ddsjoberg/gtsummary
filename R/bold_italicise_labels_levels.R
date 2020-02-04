#' Bold or Italicize labels or levels in gtsummary tables
#'
#' @name bold_italicize_labels_levels
#' @param x Object created using gtsummary functions
#' @author Daniel D. Sjoberg
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @return Functions return the same class of gtsummary object supplied
#' @examples
#' tbl_bold_ital_ex <-
#'   trial[c("trt", "age", "grade")] %>%
#'   tbl_summary() %>%
#'   bold_labels() %>%
#'   bold_levels() %>%
#'   italicize_labels() %>%
#'   italicize_levels()
#' @section Example Output:
#' \if{html}{\figure{tbl_bold_ital_ex.png}{options: width=50\%}}
#'
NULL


#' @describeIn bold_italicize_labels_levels Bold labels in gtsummary tables
#' @export
bold_labels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x)[1] %in% c(
    "tbl_summary", "tbl_regression", "tbl_uvregression",
    "tbl_stack", "tbl_merge"
  ) %>% all()) {
    stop(paste0(
      "Class of 'x' must be 'tbl_summary', 'tbl_regression', ",
      "'tbl_uvregression', 'tbl_stack', or 'tbl_merge'"
    ))
  }

  # bold labels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["bold_labels"]] <- glue(
    "gt::tab_style(style = gt::cell_text(weight = 'bold'), ",
    "locations = gt::cells_body(columns = gt::vars(label),",
    "rows = row_type == 'label'))"
  )

  x[["kable_calls"]][["bold_labels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type == 'label' ~ paste0('__', label, '__'), ",
    "TRUE ~ label",
    "))"
  )

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  x$call_list <- c(x$call_list, list(bold_labels = match.call()))

  x
}

#' @describeIn bold_italicize_labels_levels Bold levels in gtsummary tables
#' @export
bold_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x)[1] %in% c(
    "tbl_summary", "tbl_regression", "tbl_uvregression",
    "tbl_stack", "tbl_merge"
  ) %>% all()) {
    stop(paste0(
      "Class of 'x' must be 'tbl_summary', 'tbl_regression', ",
      "'tbl_uvregression', 'tbl_stack', or 'tbl_merge'"
    ))
  }

  # bold levels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["bold_levels"]] <- glue(
    "gt::tab_style(style = gt::cell_text(weight = 'bold'), ",
    "locations = gt::cells_body(columns = gt::vars(label),",
    "rows = row_type %in% c('level', 'missing')))"
  )

  x[["kable_calls"]][["bold_levels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type %in% c('level', 'missing') ~ paste0('__', label, '__'), ",
    "TRUE ~ label",
    "))"
  )

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  x$call_list <- c(x$call_list, list(bold_levels = match.call()))

  x
}


#' @describeIn bold_italicize_labels_levels Italicize labels in gtsummary tables
#' @export
italicize_labels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x)[1] %in%
    c(
      "tbl_summary", "tbl_regression", "tbl_uvregression",
      "tbl_stack", "tbl_merge"
    ) %>% all()) {
    stop(paste0(
      "Class of 'x' must be 'tbl_summary', 'tbl_regression', ",
      "'tbl_uvregression', 'tbl_stack', or 'tbl_merge'"
    ))
  }

  # italicize labels -----------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["italicize_labels"]] <- glue(
    "gt::tab_style(style = gt::cell_text(style = 'italic'), ",
    "locations = gt::cells_body(columns = gt::vars(label),",
    "rows = row_type == 'label'))"
  )

  x[["kable_calls"]][["italicize_labels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type == 'label' ~ paste0('_', label, '_'), ",
    "TRUE ~ label",
    "))"
  )

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  x$call_list <- c(x$call_list, list(italicize_labels = match.call()))

  x
}


#' @describeIn bold_italicize_labels_levels Italicize levels in gtsummary tables
#' @export
italicize_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x)[1] %in% c(
    "tbl_summary", "tbl_regression", "tbl_uvregression",
    "tbl_stack", "tbl_merge", "tbl_stack", "tbl_merge"
  ) %>% all()
  ) {
    stop(paste0(
      "Class of 'x' must be 'tbl_summary', 'tbl_regression', ",
      "'tbl_uvregression', 'tbl_stack', or 'tbl_merge'"
    ))
  }

  # italicize levels -----------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["italicize_levels"]] <- glue(
    "gt::tab_style(style = gt::cell_text(style = 'italic'), ",
    "locations = gt::cells_body(columns = gt::vars(label),",
    "rows = row_type %in% c('level', 'missing')))"
  )

  x[["kable_calls"]][["italicize_levels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type %in% c('level', 'missing') ~ paste0('_', label, '_'), ",
    "TRUE ~ label",
    "))"
  )

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  x$call_list <- c(x$call_list, list(italicize_levels = match.call()))

  x
}
