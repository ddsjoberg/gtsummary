#' Convert gtsummary object to a flextable object
#'
#' @description Function converts a gtsummary object to a flextable object.
#' A user can use this function if they wish to add customized formatting
#' available via the flextable functions. The flextable output is particularly
#' useful when combined with R markdown with Word output, since the gt package
#' does not support Word.
#'
#' @details
#' The `as_flex_table()` function supports bold and italic markdown syntax in column headers
#' and spanning headers (`'**'` and `'_'` only).
#' Text wrapped in double stars (`'**bold**'`) will be made bold, and text between single
#' underscores (`'_italic_'`) will be made italic.
#' No other markdown syntax is supported and the double-star and underscore cannot be combined.
#' To further style your table, you may convert the table to flextable with
#' `as_flex_table()`, then utilize any of the flextable functions.
#'
#' @param ... Not used
#' @inheritParams as_gt
#' @inheritParams as_tibble.gtsummary
#' @export
#' @return A 'flextable' object
#'
#' @author Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("flextable", reference_pkg = "gtsummary")
#' trial |>
#'   select(trt, age, grade) |>
#'   tbl_summary(by = trt) |>
#'   add_p() |>
#'   as_flex_table()
as_flex_table <- function(x, include = everything(), return_calls = FALSE, ...) {

}
