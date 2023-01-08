#' print and knit_print methods for gtsummary objects
#'
#' @name print_gtsummary
#' @keywords internal
#' @param x An object created using gtsummary functions
#' @param print_engine String indicating the print method. Must be one of
#' `"gt"`, `"kable"`, `"kable_extra"`, `"flextable"`, `"tibble"`
#' @param ... Not used
#' @author Daniel D. Sjoberg
#' @seealso [tbl_summary] [tbl_regression] [tbl_uvregression] [tbl_merge] [tbl_stack]
NULL

#' @rdname print_gtsummary
#' @export
print.gtsummary <- function(x, print_engine = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  # select print engine
  print_engine <-
    print_engine %||%
    .get_deprecated_option("gtsummary.print_engine") %||%
    get_theme_element("pkgwide-str:print_engine") %||%
    "gt" # default printer is gt

  # checking engine
  accepted_print_engines <-
    c("gt", "kable", "kable_extra", "flextable", "huxtable", "tibble")
  if (!rlang::is_string(print_engine) || !print_engine %in% accepted_print_engines) {
    stop(glue(
      "Select a valid print engine. ",
      "Please select one of {quoted_list(accepted_print_engines)}"
    ))
  }

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

#' @rdname print_gtsummary
#' @export
knit_print.gtsummary <- function(x, ...) {
  # assigning print engine -----------------------------------------------------
  # select print engine
  print_engine <-
    .get_deprecated_option("gtsummary.print_engine") %||%
    get_theme_element("pkgwide-str:print_engine")

  # gt is the default printer for html output
  if (is.null(print_engine) && knitr::is_html_output() == TRUE) {
    print_engine <- "gt"
  }

  # PDF uses kable as default printer (is_latex_output catches pdf_document and beamer...maybe more?)
  else if (is.null(print_engine) && knitr::is_latex_output() == TRUE) {
    rlang::inform(paste(
      "Table printed with `knitr::kable()`, not {gt}. Learn why at",
      "https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html",
      "To suppress this message, include `message = FALSE` in code chunk header.",
      sep = "\n"
    ))
    print_engine <- "kable"
  }

  # don't use word_document with gt engine
  else if (identical(print_engine %||% "gt", "gt") &&
    "docx" %in% knitr::opts_knit$get("rmarkdown.pandoc.to")) {
    if (assert_package("flextable", boolean = TRUE)) {
      rlang::inform(paste(
        "Table printed with {flextable}, not {gt}. Learn why at",
        "https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html",
        "To suppress this message, include `message = FALSE` in the code chunk header.",
        sep = "\n"
      ))
      print_engine <- "flextable"
    } else {
      rlang::inform(paste(
        "Table printed with `knitr::kable()`, not {gt}. Learn why at",
        "https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html",
        "To suppress this message, include `message = FALSE` in the code chunk header.",
        sep = "\n"
      ))
      print_engine <- "kable"
    }
  }

  # RTF warning when using gt
  else if (identical(print_engine %||% "gt", "gt") &&
    "rtf" %in% knitr::opts_knit$get("rmarkdown.pandoc.to")) {
    rlang::inform(paste(
      "Table printed with `knitr::kable()`, not {gt}. Learn why at",
      "https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html",
      "To suppress this message, include `message = FALSE` in code chunk header.",
      sep = "\n"
    ))
    print_engine <- "kable"
  }

  # all other types (if any), will attempt to print with gt
  else if (is.null(print_engine)) print_engine <- "gt"

  # printing gtsummary table with appropriate engine
  switch(print_engine,
    "gt" = as_gt(x),
    "kable" = as_kable(x),
    "flextable" = as_flex_table(x),
    "kable_extra" = as_kable_extra(x),
    "huxtable" = as_hux_table(x),
    "tibble" = as_tibble(x)
  ) %>%
    knitr::knit_print()
}
