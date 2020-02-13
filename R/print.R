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
  print_engine <-
    getOption("gtsummary.print_engine") %||%
    ifelse(
      requireNamespace("gt", quietly = TRUE),
      "gt", "kable"
    )

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
  # select print engine
  print_engine <-
    getOption("gtsummary.print_engine") %||%
    ifelse(
      requireNamespace("gt", quietly = TRUE),
      "gt", "kable"
    )

  # don't use word_document with gt engine
  if (print_engine == "gt" && "word_document" %in% rmarkdown::all_output_formats(knitr::current_input())) {
    warning(paste0(
      "\nOutput 'word_document' is not suported by the {gt} package.\n",
      "Use 'output: rtf_document' for output compatible with MS Word.\n\n",
      "Alternatively, you can use 'knitr::kable()' as the print engine and \n",
      "continue to use 'word_document'. However, some formatting may be lost,\n",
      "such as spanning table headers and footnotes.\n\n",
      "To use 'knitr::kable()' include the following code in your script:\n\n",
      "'options(gtsummary.print_engine = \"kable\")'"
    ))
  }

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

