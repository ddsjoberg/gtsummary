#' Bold or Italicize labels or levels in gtsummary tables
#'
#' @name bold_italicize_labels_levels
#' @param x an object created using gtsummary functions
#' @author Daniel D. Sjoberg
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @examples
#' tbl_bold_ital_ex <-
#'   trial %>%
#'   dplyr::select(trt, age, grade) %>%
#'   tbl_summary() %>%
#'   tab_style_bold_labels() %>%
#'   tab_style_bold_levels() %>%
#'   tab_style_italicize_labels() %>%
#'   tab_style_italicize_levels()
#' @section Example Output:
#' \if{html}{\figure{tbl_bold_ital_ex.png}{options: width=50\%}}
#'
NULL


#' @describeIn bold_italicize_labels_levels Bold labels in gtsummary tables
#' @export
tab_style_bold_labels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvregression") %>% all()) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvregression'")
  }

  # bold labels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_bold_labels"]] <- glue(
    "tab_style(style = cell_text(weight = 'bold'), ",
    "locations = cells_data(columns = vars(label),",
    "rows = row_type == 'label'))"
  )

  x[["kable_calls"]][["tab_style_bold_labels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type == 'label' ~ paste0('__', label, '__'), ",
    "TRUE ~ label",
    "))"
  )

  x$call_list <- c(x$call_list, list(tab_style_bold_labels = match.call()))

  x
}

#' @describeIn bold_italicize_labels_levels Bold levels in gtsummary tables
#' @export
tab_style_bold_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvregression") %>% all()) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvregression'")
  }

  # bold levels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_bold_levels"]] <- glue(
    "tab_style(style = cell_text(weight = 'bold'), ",
    "locations = cells_data(columns = vars(label),",
    "rows = row_type %in% c('level', 'missing')))"
  )

  x[["kable_calls"]][["tab_style_bold_levels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type %in% c('level', 'missing') ~ paste0('__', label, '__'), ",
    "TRUE ~ label",
    "))"
  )

  x$call_list <- c(x$call_list, list(tab_style_bold_levels = match.call()))

  x
}


#' @describeIn bold_italicize_labels_levels Italicize labels in gtsummary tables
#' @export
tab_style_italicize_labels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvregression") %>% all()) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvregression'")
  }

  # italicize labels ----------------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_italicize_labels"]] <- glue(
    "tab_style(style = cell_text(style = 'italic'), ",
    "locations = cells_data(columns = vars(label),",
    "rows = row_type == 'label'))"
  )

  x[["kable_calls"]][["tab_style_italicize_labels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type == 'label' ~ paste0('_', label, '_'), ",
    "TRUE ~ label",
    "))"
  )

  x$call_list <- c(x$call_list, list(tab_style_italicize_labels = match.call()))

  x
}


#' @describeIn bold_italicize_labels_levels Italicize levels in gtsummary tables
#' @export
tab_style_italicize_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!class(x) %in% c("tbl_summary", "tbl_regression", "tbl_uvregression") %>% all()) {
    stop("Class of 'x' must be 'tbl_summary', 'tbl_regression', or 'tbl_uvregression'")
  }

  # italicize levels -----------------------------------------------------------
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_italicize_levels"]] <- glue(
    "tab_style(style = cell_text(style = 'italic'), ",
    "locations = cells_data(columns = vars(label),",
    "rows = row_type %in% c('level', 'missing')))"
  )

  x[["kable_calls"]][["tab_style_italicize_levels"]] <- glue(
    "dplyr::mutate(label = dplyr::case_when(",
    "row_type %in% c('level', 'missing') ~ paste0('_', label, '_'), ",
    "TRUE ~ label",
    "))"
  )

  x$call_list <- c(x$call_list, list(tab_style_italicize_levels = match.call()))

  x
}
