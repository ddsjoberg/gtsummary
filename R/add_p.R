#' Adds P-value
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param ... Passed to other methods.
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @export
add_p <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_p")
}

add_p.tbl_summary <- function(x,
                              test = NULL,
                              pvalue_fun = styfn_pvalue(),
                              group = NULL,
                              include = everything(),
                              test.args = NULL,
                              ...) {
  # check/process inputs -------------------------------------------------------
  check_dots_empty()
  # checking that input x has a by var
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_p} when {.code tbl_summary(by)} argument not included." |>
      cli::cli_abort()
  }

  cards::process_selectors(
    select_prep(x$table_body, x$inputs$data),
    include = {{ include }}
  )
  cards::process_selectors(x$inputs$data, group = {{ group }})
  check_scalar(group, allow_empty = TRUE)

  # if `pvalue_fun` not modified, check if we need to use a theme p-value
  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun") %||%
      pvalue_fun
  }
  pvalue_fun <- as_function(pvalue_fun)
}
