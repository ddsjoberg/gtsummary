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
    rlang::inform(glue(
      "The {gt} package is not installed and the table will be printed using\n",
      "`knitr::kable()`, which prints a simpler table compared to {gt}.\n",
      "For example, `knitr::kable()` does not support footnotes,\n",
      "spanning headers, or indentation.\n\n",

      "For tables styled by the {gt} package, use the installation code below.\n",
      "`remotes::install_github(\"rstudio/gt\", ref = gtsummary::gt_sha)`\n\n",

      "If you prefer to always use `knitr::kable()`, add the option\n",
      "`options(gtsummary.print_engine = \"kable\")` to your script."
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
        "Table will be printed with `knitr::kable()`, which is less featured than {gt}.\n",
        "For more information on {gtsummary} in R markdown, review the vignette (link below).",
        "http://www.danieldsjoberg.com/gtsummary/dev/articles/gtsummary_with_rmarkdown.html \n\n",
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
        "Output 'word_document' is not suported by the {gt} package. Table will be\n",
        "printed with `knitr::kable()`, which is less featured than {gt}.\n",
        "For more information on {gtsummary} in R markdown, review the vignette (link below).\n",
        "http://www.danieldsjoberg.com/gtsummary/dev/articles/gtsummary_with_rmarkdown.html \n\n",
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
        "Output 'rtf_document' is in development by the {gt} package, and tables may\n",
        "contain malformed elements or may not print at all.\n",
        "For more information on {gtsummary} in R markdown, review the vignette (link below).\n",
        "http://www.danieldsjoberg.com/gtsummary/dev/articles/gtsummary_with_rmarkdown.html \n\n",
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
    rlang::inform(paste0(
      "The {gt} package is not installed and the table will be printed using\n",
      "`knitr::kable()`, which prints a simpler table compared to {gt}.\n",
      "For example, `knitr::kable()` does not support footnotes,\n",
      "spanning headers, or indentation.\n\n",

      "For tables styled by the {gt} package, use the installation code below.\n",
      "`remotes::install_github(\"rstudio/gt\", ref = gtsummary::gt_sha)`\n\n",

      "If you prefer to always use `knitr::kable()`, add the option\n",
      "`options(gtsummary.print_engine = \"kable\")` to your script."
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

