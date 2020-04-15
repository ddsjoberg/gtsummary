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

  # default printer is gt
  if (is.null(print_engine)) print_engine <- "gt"

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

  # gt is the default printer for html output
  if (is.null(print_engine) && knitr::is_html_output() == TRUE) {
    print_engine <- "gt"
  }

  # PDF uses kable as default printer (is_latex_output catches pdf_document and beamer...maybe more?)
  else if (is.null(print_engine) && knitr::is_latex_output() == TRUE) {
    rlang::inform(paste(
        "Table printed with `knitr::kable()`, not {gt}. Learn why at",
        "http://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html",
        "To suppress this message, include `message = FALSE` in code chunk header.",
        sep = "\n"
    ))
    print_engine <- "kable"
  }

  # don't use word_document with gt engine
  else if (identical(print_engine %||% "gt", "gt") &&
           "docx" %in% knitr::opts_knit$get('rmarkdown.pandoc.to')) {
    rlang::inform(paste(
      "Table printed with `knitr::kable()`, not {gt}. Learn why at",
      "http://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html",
      "To suppress this message, include `message = FALSE` in the code chunk header.",
      sep = "\n"
    ))
    print_engine <- "kable"
  }

  # RTF warning when using gt
  else if (identical(print_engine %||% "gt", "gt") &&
           "rtf" %in% knitr::opts_knit$get('rmarkdown.pandoc.to')) {
    rlang::inform(paste(
      "Table printed with `knitr::kable()`, not {gt}. Learn why at",
      "http://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html",
      "To suppress this message, include `message = FALSE` in code chunk header.",
      sep = "\n"
    ))
    print_engine <- "kable"
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

