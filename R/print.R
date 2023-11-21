#' print and knit_print methods for gtsummary objects
#'
#' @name print_gtsummary
#' @keywords internal
#' @param x An object created using gtsummary functions
#' @param print_engine String indicating the print method. Must be one of
#' `"gt"`, `"kable"`, `"kable_extra"`, `"flextable"`, `"tibble"`
#' @param ... Not used
#' @author Daniel D. Sjoberg
NULL

#' @rdname print_gtsummary
#' @export
print.gtsummary <- function(x, print_engine = "gt", ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  # # select print engine
  # print_engine <-
  #   print_engine %||%
  #   .get_deprecated_option("gtsummary.print_engine") %||%
  #   # get_theme_element("pkgwide-str:print_engine") %||%
  #   "gt" # default printer is gt
  #
  # # checking engine
  # accepted_print_engines <-
  #   c("gt", "kable", "kable_extra", "flextable", "huxtable", "tibble")
  # if (!rlang::is_string(print_engine) || !print_engine %in% accepted_print_engines) {
  #   stop(glue(
  #     "Select a valid print engine. ",
  #     "Please select one of {quoted_list(accepted_print_engines)}"
  #   ))
  # }

  # printing results
  switch(print_engine,
    "gt" = as_gt(x),
    "kable" = as_kable(x),
    "flextable" = as_flex_table(x),
    "kable_extra" = as_kable_extra(x),
    "huxtable" = as_hux_table(x),
    "tibble" = as_tibble(x)
  ) %>%
    print()
}

