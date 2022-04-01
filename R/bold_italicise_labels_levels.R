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
  updated_call_list <- c(x$call_list, list(bold_labels = match.call()))
  # input checks ---------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # bold labels ----------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = "label",
      rows = .data$row_type == "label",
      text_format = "bold"
    )

  x$call_list <- updated_call_list

  x
}

#' @describeIn bold_italicize_labels_levels Bold levels in gtsummary tables
#' @export
bold_levels <- function(x) {
  updated_call_list <- c(x$call_list, list(bold_levels = match.call()))
  # input checks ---------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # bold levels ----------------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = "label",
      rows = .data$row_type != "label",
      text_format = "bold"
    )

  x$call_list <- updated_call_list

  x
}


#' @describeIn bold_italicize_labels_levels Italicize labels in gtsummary tables
#' @export
italicize_labels <- function(x) {
  updated_call_list <- c(x$call_list, list(italicize_labels = match.call()))
  # input checks ---------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # italicize labels -----------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = "label",
      rows = .data$row_type == "label",
      text_format = "italic"
    )

  x$call_list <- updated_call_list

  x
}


#' @describeIn bold_italicize_labels_levels Italicize levels in gtsummary tables
#' @export
italicize_levels <- function(x) {
  updated_call_list <- c(x$call_list, list(italicize_levels = match.call()))
  # input checks ---------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # italicize levels -----------------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = "label",
      rows = .data$row_type != "label",
      text_format = "italic"
    )

  x$call_list <- updated_call_list

  x
}
