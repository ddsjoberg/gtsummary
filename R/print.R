#' print and knit_print methods for gtsummary objects
#'
#' @name print_gtsummary
#' @param x an object created using gtsummary functions
#' @param ... not used
#' @author Daniel D. Sjoberg
#' @seealso \link{tbl_summary} \link{tbl_regression} \link{tbl_uvregression}
NULL

#' @rdname print_gtsummary
#' @export
print.tbl_summary <- function(x, ...) as_gt(x) %>% print()

#' @rdname print_gtsummary
#' @export
knit_print.tbl_summary <- function(x, ...) {
  if ("word_document" %in% rmarkdown::all_output_formats(knitr::current_input())) {
    warning("Output 'word_document' is not suported by the {gt} package. Use 'output: rtf_document' for output compatible with MS Word.")
  }
  as_gt(x) %>% knitr::knit_print()
}

#' @rdname print_gtsummary
#' @export
print.tbl_regression <- print.tbl_summary

#' @rdname print_gtsummary
#' @export
knit_print.tbl_regression <- knit_print.tbl_summary

#' @rdname print_gtsummary
#' @export
print.tbl_uvregression <- print.tbl_summary

#' @rdname print_gtsummary
#' @export
knit_print.tbl_uvregression <- knit_print.tbl_summary

#' @rdname print_gtsummary
#' @export
print.tbl_survival <- print.tbl_summary

#' @rdname print_gtsummary
#' @export
knit_print.tbl_survival <- knit_print.tbl_summary

#' @rdname print_gtsummary
#' @export
print.tbl_merge <- print.tbl_summary

#' @rdname print_gtsummary
#' @export
knit_print.tbl_merge <- knit_print.tbl_summary

#' @rdname print_gtsummary
#' @export
print.tbl_stack <- print.tbl_summary

#' @rdname print_gtsummary
#' @export
knit_print.tbl_stack <- knit_print.tbl_summary
