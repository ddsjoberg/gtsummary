#' Convert gtsummary object to a huxtable object
#'
#' Function converts a gtsummary object to a huxtable object.
#' A user can use this function if they wish to add customized formatting
#' available via the huxtable functions. The huxtable package supports output
#' to PDF via LaTeX, as well as HTML and Word.
#'
#' @section Excel Output:
#'
#' Use the `as_hux_xlsx()` function to save a copy of the table in an excel file.
#' The file is saved using `huxtable::quick_xlsx()`.
#'
#' @inheritParams as_flex_table
#' @inheritParams huxtable::quick_xlsx
#' @param bold_header_rows logical indicating whether to bold header rows.
#' Default is `TRUE`
#' @param strip_md_bold DEPRECATED
#' @name as_hux_table
#' @return A \{huxtable\} object
#'
#' @author David Hugh-Jones, Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("huxtable", reference_pkg = "gtsummary")
#' trial |>
#'   dplyr::select(trt, age, grade) |>
#'   tbl_summary(by = trt) |>
#'   add_p() |>
#'   as_hux_table()
NULL

#' @export
#' @rdname as_hux_table
as_hux_table <- function(x, include = everything(), return_calls = FALSE,
                         strip_md_bold = FALSE) {
}

#' @export
#' @rdname as_hux_table
as_hux_xlsx <- function(x, file, include = everything(), bold_header_rows = TRUE) {
}
