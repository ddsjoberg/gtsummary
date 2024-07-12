#' Add multiple comparison adjustment
#'
#' Adjustments to p-values are performed with [`stats::p.adjust()`].
#'
#' @param x (`gtsummary`)\cr
#'   a `gtsummary` object with a column named `"p.value"`
#' @param method (`string`)\cr
#'   String indicating method to be used for p-value adjustment. Methods from
#'   [`stats::p.adjust()`] are accepted.  Default is `method='fdr'`.
#'   Must be one of `r shQuote(stats::p.adjust.methods, "sh")`
#' @param pvalue_fun (`function`)\cr
#'   Function to round and format q-values. Default is the function specified
#'   to round the existing `'p.value'` column.
#' @param quiet `r lifecycle::badge("deprecated")`
#'
#' @author Daniel D. Sjoberg, Esther Drill
#' @export
#' @examplesIf gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")
#' # Example 1 ----------------------------------
#' add_q_ex1 <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
#'   add_p() |>
#'   add_q()
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   tbl_uvregression(
#'     y = response,
#'     include = c("trt", "age", "grade"),
#'     method = glm,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) |>
#'   add_global_p() |>
#'   add_q()
add_q <- function(x, method = "fdr", pvalue_fun = NULL, quiet = NULL) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(add_q = match.call()))

  # deprecation ----------------------------------------------------------------
  if (!is_empty(quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::add_q(quiet)"
    )
  }

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  method <- arg_match(method, values = stats::p.adjust.methods, multiple = TRUE)
  # checking input table has a p.value column
  if (!"p.value" %in% names(x$table_body)) {
    cli::cli_abort(
      "There is no p-value column. `x$table_body` must have a column called {.val p.value}.",
      call = get_cli_abort_call()
    )
  }

  # setting defaults from gtsummary theme --------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    # defaults from theme
    get_theme_element("add_q-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    # default from p-value formatting function
    (dplyr::filter(x$table_styling$fmt_fun, .data$column == "p.value") |> dplyr::pull("fmt_fun") |> rev() |> getElement(1)) |>
    as_function(arg = "pvalue_fun")

  # calculate the adjusted p-value ---------------------------------------------
  q.value <- stats::p.adjust(x$table_body$p.value, method = method)

  # update gtsummary table -----------------------------------------------------
  x <-
    modify_table_body(x, ~ dplyr::mutate(.x, q.value = q.value)) |>
    modify_table_styling(
      columns = "q.value",
      label = paste0("**", translate_string("q-value"), "**"),
      hide = FALSE,
      footnote = .add_q_method_label(method),
      fmt_fun = pvalue_fun
    )

  # return final object --------------------------------------------------------
  x |>
    .fill_table_header_modify_stats()
}


# match method input to display name
.add_q_method_label <- function(method) {
  lst_method_labels <-
    list(
      "holm" = "Holm correction for multiple testing",
      "hochberg" = "Hochberg correction for multiple testing",
      "hommel" = "Hommel correction for multiple testing",
      "bonferroni" = "Bonferroni correction for multiple testing",
      "BH" = "Benjamini & Hochberg correction for multiple testing",
      "BY" = "Benjamini & Yekutieli correction for multiple testing",
      "fdr" = "False discovery rate correction for multiple testing",
      "none" = "No correction for multiple testing"
    )

  lst_method_labels[[method]] |>
    translate_string()
}
