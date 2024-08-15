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
  set_cli_abort_call()
  check_dots_empty()
  print_engine <-
    case_switch(
      missing(print_engine) ~ get_theme_element("pkgwide-str:print_engine", default = print_engine),
      .default = print_engine
    )
  print_engine <- arg_match(print_engine, values = c("gt", "flextable", "huxtable", "kable", "kable_extra", "tibble"))

  # printing results
  res <- switch(
    print_engine,
    "gt" = as_gt(x),
    "kable" = as_kable(x),
    "flextable" = as_flex_table(x),
    "kable_extra" = as_kable_extra(x),
    "huxtable" = as_hux_table(x),
    "tibble" = as_tibble(x)
  )

  print(res)
  invisible(res)
}

#' @rdname print_gtsummary
#' @exportS3Method knitr::knit_print
knit_print.gtsummary <- function(x,
                                 print_engine = c("gt", "flextable", "huxtable", "kable", "kable_extra", "tibble"), ...) {
  set_cli_abort_call()

  print_engine <-
    case_switch(
      missing(print_engine) ~ get_theme_element("pkgwide-str:print_engine", default = print_engine),
      .default = print_engine
    )
  print_engine <- arg_match(print_engine, values = c("gt", "flextable", "huxtable", "kable", "kable_extra", "tibble"))

  # printing results
  res <- switch(
    print_engine,
    "gt" = as_gt(x),
    "kable" = as_kable(x),
    "flextable" = as_flex_table(x),
    "kable_extra" = as_kable_extra(x),
    "huxtable" = as_hux_table(x),
    "tibble" = as_tibble(x)
  )

  # remove_line_breaks
  if (print_engine == "gt" && knitr::is_latex_output()) {
    res <- res |>
        gt::cols_label_with(fn = function(x) str_replace_all(x, pattern = "\\n(?!\\\\)", replacement = ""))
  }

  knitr::knit_print(res)
}

#' @rdname print_gtsummary
#' @export
pkgdown_print.gtsummary <- function(x, visible = TRUE) {
  check_installed("htmltools")

  if (!visible) {
    return(invisible())
  }

  call2("div", class = "gt-table", gtsummary::as_gt(x) |> gt::as_raw_html(), .ns = "htmltools") |>
    eval_tidy()
}
