#' print and knit_print methods for gtsummary objects
#'
#' @name print_gtsummary
#' @param x An object created using gtsummary functions
#' @param ... Not used
#' @author Daniel D. Sjoberg
#' @seealso [tbl_summary] [tbl_regression] [tbl_uvregression] [tbl_merge] [tbl_stack]
NULL

#' @rdname print_gtsummary
#' @export
print.gtsummary <- function(x, ...) {
  # select print engine
  print_engine <- getOption("gtsummary.print_engine")

  # print message about installing gt if not installed, and table would have been printed with gt
  if ((is.null(print_engine) | print_engine == "gt") && !requireNamespace("gt", quietly = TRUE)) {
    rlang::inform(paste(
      "Install {gt} with `remotes::install_github(\"rstudio/gt\", ref = gtsummary::gt_sha)`",
      "As {gt} package is not installed, table was printed using `knitr::kable()`. Details at",
      "http://www.danieldsjoberg.com/gtsummary/dev/articles/print.html",
      sep = "\n"
    ))

    print_engine <- "kable"
  }
  else if (is.null(print_engine)) print_engine <- "gt"

  # printing results
  if (print_engine == "gt") {
    return(as_gt(x) %>% print())
  } else if (print_engine == "kable") {
    return(as_kable(x) %>% print())
  } else {
    stop(glue(
      "'{print_engine}' is not a valid print engine. ",
      "Please select 'gt' or 'kable' in 'options(gtsummary.print_engine = \"gt\")'"
    ))
  }
}

#' @rdname print_gtsummary
#' @export
knit_print.gtsummary <- function(x, ...) {
  # assigning print engine -----------------------------------------------------
  # select print engine
  print_engine <- getOption("gtsummary.print_engine")

  # print message about installing gt if not installed, and table would have been printed with gt
  if ((is.null(print_engine) | print_engine == "gt") && !requireNamespace("gt", quietly = TRUE)) {
    rlang::inform(paste(
      "Install {gt} with `remotes::install_github(\"rstudio/gt\", ref = gtsummary::gt_sha)`",
      "As {gt} package is not installed, table was printed using `knitr::kable()`. Details at",
      "http://www.danieldsjoberg.com/gtsummary/dev/articles/print.html",
      "To supress this message, include `message = FALSE` in code chunk header.",
      sep = "\n"
    ))
    print_engine <- "kable"
  }

  # gt is the default printer for html output
  else if (is.null(print_engine) && knitr::is_html_output() == TRUE) {
    print_engine <- "gt"
  }

  # PDF uses kable as default printer (is_latex_output catches pdf_document and beamer...maybe more?)
  else if (is.null(print_engine) && knitr::is_latex_output() == TRUE) {
    rlang::inform(paste(
        "Table will be printed with `knitr::kable()`. Details at",
        "http://www.danieldsjoberg.com/gtsummary/dev/articles/print.html",
        "To supress this message, include `message = FALSE` in code chunk header.",
        sep = "\n"
    ))
    print_engine <- "kable"
  }

  # don't use word_document with gt engine
  else if ((is.null(print_engine) | print_engine == "gt") &&
           "word_document" %in% rmarkdown::all_output_formats(knitr::current_input())) {
    rlang::inform(paste(
      "Table will be printed with `knitr::kable()`. Details at",
      "http://www.danieldsjoberg.com/gtsummary/dev/articles/print.html",
      "To supress this message, include `message = FALSE` in the code chunk header.",
      sep = "\n"
    ))
    print_engine <- "kable"
  }

  # RTF warning when using gt
  else if ((is.null(print_engine) | print_engine == "gt") &&
           "rtf_document" %in% rmarkdown::all_output_formats(knitr::current_input())) {
    rlang::inform(paste(
        "Output 'rtf_document' is in development by the {gt} package. Details at",
        "http://www.danieldsjoberg.com/gtsummary/dev/articles/print.html \n\n",
        "To supress this message, include `message = FALSE` in the code chunk header.",
        sep = "\n"
      ))
    print_engine <- "gt"
  }

  # all other types (if any), will attempt to print with gt
  else if (is.null(print_engine)) print_engine <- "gt"

  # printing results
  if (print_engine == "gt") {
    return(as_gt(x) %>% knitr::knit_print())
  } else if (print_engine == "kable") {
    return(as_kable(x) %>% knitr::knit_print())
  } else {
    stop(glue(
      "'{print_engine}' is not a valid print engine. ",
      "Please select 'gt' or 'kable' in 'options(gtsummary.print_engine = \"gt\")'"
    ))
  }
}

