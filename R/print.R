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
  if (is.null(print_engine) && !requireNamespace("gt", quietly = TRUE)) {
    rlang::inform(glue(
      "Results will be printed using `knitr::kable()` and do not \n",
      "support footnotes, spanning headers, or indentation. \n\n",
      "For tables styled by the gt package, use the installation code below.\n",
      "`remotes::install_github(\"rstudio/gt\", ref = gtsummary::gt_sha)`\n\n",
      "If you prefer to always use `knitr::kable()`, add the option\n",
      "`options(gtsummary.print_engine = \"kable\")` to your script\n",
      "or in a user- or project-level startup file, '.Rprofile'."
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

  # gt is the default printer for html output
  if (is.null(print_engine) && knitr::is_html_output() == TRUE) {
    print_engine <- "gt"
  }

  # PDF uses kable as default printer (is_latex_output catches pdf_document and beamer output...maybe more?)
  else if (is.null(print_engine) && knitr::is_latex_output() == TRUE) {
    rlang::inform(
      message = paste0(
        "The {gtsummary} package was built to print tables with the {gt} package.\n",
        "Output to PDF is in development by the {gt} package.\n",
        "Table will be printed with `knitr::kable()`, which is less featured than {gt}.\n",
        "To print table with the {gt} package, convert {gtsummary} object to {gt} with `as_gt()`\n",
        "  or include `options(gtsummary.print_engine = \"gt\")` at the top of your script.\n",
        "For more information on {gtsummary} in R markdown, review vignette.\n",
        "To supress this message include, `message = FALSE` in the code chunk header."
      )
    )
    print_engine <- "kable"
  }

  # don't use word_document with gt engine
  else if ((is.null(print_engine) | print_engine == "gt") &&
           "word_document" %in% rmarkdown::all_output_formats(knitr::current_input())) {
    rlang::inform(
      message = paste0(
        "The {gtsummary} package was built to print tables with the {gt} package.\n",
        "Output 'word_document' is not suported by the {gt} package.\n",
        "Table will be printed with `knitr::kable()`, which is less featured than {gt}.\n",
        "For more information on {gtsummary} in R markdown, review vignette.\n",
        "To supress this message include, `message = FALSE` in the code chunk header."
      )
    )
    print_engine <- "kable"
  }

  # RTF warning when using gt
  else if ((is.null(print_engine) | print_engine == "gt") &&
           "rtf_document" %in% rmarkdown::all_output_formats(knitr::current_input())) {
    rlang::inform(
      message = paste0(
        "The {gtsummary} package was built to print tables with the {gt} package.\n",
        "Output 'rtf_document' is in development by the {gt} package, and tables may contain malformed elements.\n",
        "For more information on {gtsummary} in R markdown, review vignette.\n",
        "To supress this message include, `message = FALSE` in the code chunk header."
      )
    )
    print_engine <- "gt"
  }

  # all other types (if any), will attempt to print with gt
  else if (is.null(print_engine)) print_engine <- "gt"

  # need to install gt? --------------------------------------------------------
  # print message about installing gt if not installed, and table would have been printed with gt
  if (print_engine == "gt" && !requireNamespace("gt", quietly = TRUE)) {
    rlang::inform(glue(
      "Results will be printed using `knitr::kable()` and do not \n",
      "support footnotes, spanning headers, or indentation. \n\n",
      "For tables styled by the gt package, use the installation code below.\n",
      "`remotes::install_github(\"rstudio/gt\", ref = gtsummary::gt_sha)`\n\n",
      "If you prefer to always use `knitr::kable()`, add the option\n",
      "`options(gtsummary.print_engine = \"kable\")` to your script\n",
      "or in a user- or project-level startup file, '.Rprofile'."
    ))
    print_engine <- "kable"
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

