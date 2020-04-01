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
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }

  # bold labels ----------------------------------------------------------------
  x$table_header <-
    x$table_header %>%
    mutate(
      bold = case_when(
        .data$column == "label" & is.na(bold) ~ "row_type == 'label'",
        .data$column == "label" & !is.na(bold) ~ paste(bold, "row_type == 'label'", sep = " | "),
        TRUE ~ bold
      )
    )

  x$call_list <- c(x$call_list, list(bold_labels = match.call()))

  x
}

#' @describeIn bold_italicize_labels_levels Bold levels in gtsummary tables
#' @export
bold_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }

  # bold levels ----------------------------------------------------------------
  x$table_header <-
    x$table_header %>%
    mutate(
      bold = case_when(
        .data$column == "label" & is.na(bold) ~ "row_type != 'label'",
        # appending condition to existing condition
        .data$column == "label" & !is.na(bold) ~ paste(bold, "row_type != 'label'", sep = " | "),
        TRUE ~ bold
      )
    )

  x$call_list <- c(x$call_list, list(bold_levels = match.call()))

  x
}


#' @describeIn bold_italicize_labels_levels Italicize labels in gtsummary tables
#' @export
italicize_labels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }

  # italicize labels -----------------------------------------------------------
  x$table_header <-
    x$table_header %>%
    mutate(
      italic = case_when(
        .data$column == "label" & is.na(italic) ~ "row_type == 'label'",
        .data$column == "label" & !is.na(italic) ~ paste(italic, "row_type == 'label'", sep = " | "),
        TRUE ~ italic
      )
    )

  x$call_list <- c(x$call_list, list(italicize_labels = match.call()))

  x
}


#' @describeIn bold_italicize_labels_levels Italicize levels in gtsummary tables
#' @export
italicize_levels <- function(x) {
  # input checks ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Class of 'x' must be 'gtsummary'", call. = FALSE)
  }

  # italicize levels -----------------------------------------------------------
  x$table_header <-
    x$table_header %>%
    mutate(
      italic = case_when(
        .data$column == "label" & is.na(italic) ~ "row_type != 'label'",
        # appending condition to existing condition
        .data$column == "label" & !is.na(italic) ~ paste(italic, "row_type != 'label'", sep = " | "),
        TRUE ~ italic
      )
    )

  x$call_list <- c(x$call_list, list(italicize_levels = match.call()))

  x
}
