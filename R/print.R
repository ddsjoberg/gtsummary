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
print.gtsummary <- function(x,
                            print_engine = c("gt", "flextable", "huxtable", "kable", "kable_extra", "tibble"),
                            ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  print_engine <-
    if (missing(print_engine)) get_theme_element("pkgwide-str:print_engine") %||% print_engine else print_engine
  print_engine <- arg_match(print_engine)

  # printing results
  switch(print_engine,
    "gt" = as_gt(x)
    # ,
    # "kable" = as_kable(x),
    # "flextable" = as_flex_table(x),
    # "kable_extra" = as_kable_extra(x),
    # "huxtable" = as_hux_table(x),
    # "tibble" = as_tibble(x)
  ) %>%
    print()
}
