#' Bold or Italicize
#'
#' Bold or italicize labels or levels in gtsummary tables
#'
#' @name bold_italicize_labels_levels
#' @param x (`gtsummary`)
#'   An object of class 'gtsummary'
#' @author Daniel D. Sjoberg
#'
#' @return Functions return the same class of gtsummary object supplied
#' @examples
#' # Example 1 ----------------------------------
#' tbl_summary(trial, include = c("trt", "age", "response")) |>
#'   bold_labels() |>
#'   bold_levels() |>
#'   italicize_labels() |>
#'   italicize_levels()
NULL


#' @export
#' @rdname bold_italicize_labels_levels
bold_labels <- function(x) {
  UseMethod("bold_labels")
}

#' @export
#' @rdname bold_italicize_labels_levels
italicize_labels <- function(x) {
  UseMethod("italicize_labels")
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_levels <- function(x) {
  UseMethod("bold_levels")
}


#' @export
#' @rdname bold_italicize_labels_levels
italicize_levels <- function(x) {
  UseMethod("italicize_levels")
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_labels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(bold_labels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code bold_labels()} cannot be used in this context.")
    return(x)
  }

  # bold labels ----------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type == "label",
      text_format = "bold"
    )

  x$call_list <- updated_call_list

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_levels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(bold_levels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code bold_levels()} cannot be used in this context.")
    return(x)
  }

  # bold levels ----------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type != "label",
      text_format = "bold"
    )

  x$call_list <- updated_call_list

  x
}


#' @export
#' @rdname bold_italicize_labels_levels
italicize_labels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(italicize_labels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code italicize_labels()} cannot be used in this context.")
    return(x)
  }

  # italicize labels -----------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type == "label",
      text_format = "italic"
    )

  x$call_list <- updated_call_list

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
italicize_levels.gtsummary <- function(x) {
  updated_call_list <- c(x$call_list, list(italicize_levels = match.call()))
  # input checks ---------------------------------------------------------------
  if (!"row_type" %in% x$table_styling$header$column) {
    cli::cli_inform("{.code italicize_levels()} cannot be used in this context.")
    return(x)
  }

  # italicize levels -----------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = .first_unhidden_column(x),
      rows = .data$row_type != "label",
      text_format = "italic"
    )

  x$call_list <- updated_call_list

  x
}


.first_unhidden_column <- function(x) {
  x$table_styling$header |>
    dplyr::filter(!.data$hide) |>
    dplyr::pull("column") |>
    dplyr::first()
}
