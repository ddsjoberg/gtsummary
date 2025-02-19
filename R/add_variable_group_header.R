#' Variable Group Header
#'
#' @description
#' Some data are inherently grouped, and should be reported together.
#' Grouped variables are all indented together.
#' This function indents the variables that should be reported together while
#' adding a header above the group.
#'
#' @inheritParams modify_column_indent
#' @param x (`tbl_summary`)\cr
#'   gtsummary object of class `'tbl_summary'`
#' @param header (`string`)\cr
#'   string of the header to place above the variable group
#' @param variables  ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to group that appear in `x$table_body`.
#'   Selected variables should be appear consecutively in table.
#'
#' @returns a gtsummary table
#' @export
#'
#' @details
#' This function works by inserting a row into the `x$table_body` and
#' indenting the group of selected variables.
#' This function cannot be used in conjunction with all functions in gtsummary;
#' for example, `bold_labels()` will bold the incorrect rows after running
#' this function.
#'
#' @examples
#' # Example 1 ----------------------------------
#' set.seed(11234)
#' data.frame(
#'   exclusion_age = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'   exclusion_mets = sample(c(TRUE, FALSE), 20, replace = TRUE),
#'   exclusion_physician = sample(c(TRUE, FALSE), 20, replace = TRUE)
#' ) |>
#'   tbl_summary(
#'     label = list(exclusion_age = "Age",
#'                  exclusion_mets = "Metastatic Disease",
#'                  exclusion_physician = "Physician")
#'   ) |>
#'   add_variable_group_header(
#'     header = "Exclusion Reason",
#'     variables = starts_with("exclusion_")
#'   ) |>
#'   modify_caption("**Study Exclusion Criteria**")
add_variable_group_header <- function(x, header, variables, indent = 4L) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "tbl_summary")
  check_string(header)
  check_scalar_integerish(indent)

  # process variables ----------------------------------------------------------
  cards::process_selectors(scope_table_body(x$table_body), variables = {{ variables }})
  # return unaltered table if no variables selected
  if (is_empty(variables)) return(x) # styler: off

  # add header above the first variable selected -------------------------------
  df_insert <- dplyr::tibble(row_type = "variable_group", label = header)
  # identify row where to insert header
  idx <- which(x$table_body$variable %in% variables)[1]

  x |>
    modify_table_body(~ dplyr::add_row(.x, df_insert, .before = idx)) |>
    .modify_column_indent(
      columns = "label",
      rows = .data$variable %in% .env$variables,
      indent = indent
    )
}
