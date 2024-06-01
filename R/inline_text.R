#' Report statistics from gtsummary tables inline
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @return A string reporting results from a gtsummary table
#' @keywords internal
#' @author Daniel D. Sjoberg
#'
#' @export
inline_text <- function(x, ...) {
  UseMethod("inline_text")
}
