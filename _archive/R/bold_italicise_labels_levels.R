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
#' # Example 1 ----------------------------------
#' tbl_bold_ital_ex1 <-
#'   trial[c("trt", "age", "grade")] %>%
#'   tbl_summary() %>%
#'   bold_labels() %>%
#'   bold_levels() %>%
#'   italicize_labels() %>%
#'   italicize_levels()
#' @section Example Output:
#'
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_bold_ital_ex1.png", width = "50")`
#' }}
#'
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
    cli::cli_alert_warning("{.code bold_labels()} cannot be used in this context.")
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
    cli::cli_alert_warning("{.code bold_levels()} cannot be used in this context.")
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
    cli::cli_alert_warning("{.code italicize_labels()} cannot be used in this context.")
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
    cli::cli_alert_warning("{.code italicize_levels()} cannot be used in this context.")
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

#' @export
#' @rdname bold_italicize_labels_levels
bold_labels.tbl_cross <- function(x) {
  # bold labels ----------------------------------------------------------------
  x <- bold_labels.gtsummary(x)

  cols_to_style <-
    select(x$table_body, all_stat_cols(FALSE)) %>%
    names()

  x$table_styling$header <-
    mutate(x$table_styling$header,
      spanning_header =
        case_when(
          .data$hide == FALSE & (.data$column %in% cols_to_style) ~
            paste0("**", spanning_header, "**"),
          TRUE ~ spanning_header
        )
    ) %>%
    mutate(label = case_when(
      .data$hide == FALSE & (.data$column %in% c("stat_0", "p.value")) ~
        paste0("**", label, "**"),
      TRUE ~ label
    ))

  x
}

#' @export
#' @rdname bold_italicize_labels_levels
bold_levels.tbl_cross <- function(x) {
  # bold levels ----------------------------------------------------------------
  x <- bold_levels.gtsummary(x)

  cols_to_style <- x$table_body %>%
    select(all_stat_cols(FALSE)) %>%
    names()

  x$table_styling$header <-
    mutate(x$table_styling$header,
      label =
        case_when(
          .data$hide == FALSE & (.data$column %in% cols_to_style) ~
            paste0("**", label, "**"),
          TRUE ~ label
        )
    )



  x
}

#' @export
#' @rdname bold_italicize_labels_levels
italicize_labels.tbl_cross <- function(x) {
  # italicize labels -----------------------------------------------------------
  x <- italicize_labels.gtsummary(x)

  cols_to_style <-
    select(x$table_body, all_stat_cols(FALSE)) %>%
    names()

  x$table_styling$header <-
    mutate(x$table_styling$header,
      spanning_header =
        case_when(
          .data$hide == FALSE & (.data$column %in% cols_to_style) ~
            paste0("*", spanning_header, "*"),
          TRUE ~ spanning_header
        )
    ) %>%
    mutate(label = case_when(
      .data$hide == FALSE & (.data$column %in% c("stat_0", "p.value")) ~
        paste0("*", label, "*"),
      TRUE ~ label
    ))

  x
}



#' @export
#' @rdname bold_italicize_labels_levels
italicize_levels.tbl_cross <- function(x) {
  x <- italicize_levels.gtsummary(x)

  # italicize levels ----------------------------------------------------------------
  cols_to_style <- x$table_body %>%
    select(all_stat_cols(FALSE)) %>%
    names()

  x$table_styling$header <-
    mutate(x$table_styling$header,
      label =
        case_when(
          .data$hide == FALSE & (.data$column %in% cols_to_style) ~
            paste0("*", label, "*"),
          TRUE ~ label
        )
    )
  x
}

.first_unhidden_column <- function(x) {
  x$table_styling$header %>%
    dplyr::filter(!.data$hide) %>%
    purrr::pluck("column", 1)
}
