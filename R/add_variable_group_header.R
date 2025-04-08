#' Variable Group Header
#'
#' @description
#' Some data are inherently grouped, and should be reported together.
#' Grouped variables are all indented together.
#' This function indents the variables that should be reported together while
#' adding a header above the group.
#'
#' @param x (`tbl_summary`)\cr
#'   gtsummary object of class `'tbl_summary'`
#' @param header (`string`)\cr
#'   string of the header to place above the variable group
#' @param variables  ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to group that appear in `x$table_body`.
#'   Selected variables should be appear consecutively in table.
#' @param indent (`integer`)\cr
#'   An integer indicating how many space to indent text.
#'   All rows in the group will be indented by this amount. Default is `4`.
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
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "car", "broom", "broom.helpers", "parameters"))
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
#'
#' # Example 2 ----------------------------------
#' lm(marker ~ trt + grade + age, data = trial) |>
#'   tbl_regression() |>
#'   add_global_p(keep = TRUE, include = grade) |>
#'   add_variable_group_header(
#'     header = "Treatment:",
#'     variables = trt
#'   ) |>
#'   add_variable_group_header(
#'     header = "Covariate:",
#'     variables = -trt
#'   ) |>
#'   # indent levels 8 spaces
#'   modify_column_indent(
#'     columns = "label",
#'     rows = row_type == "level",
#'     indent = 8L
#'   )
add_variable_group_header <- function(x, header, variables, indent = 4L) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "gtsummary")
  check_string(header)
  check_scalar_integerish(indent)

  # check the necessary structure is in place
  if (!all(c("variable", "row_type", "label") %in% names(x$table_body)) ||
      .first_unhidden_column(x) != "label") {
    cli::cli_abort(
      "The {.cls gtsummary} table must include columns {.val {c('variable', 'row_type', 'label')}}
       in the {.code x$table_body} data frame and the {.val label} column must appear first.",
      call = get_cli_abort_call()
    )
  }

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
