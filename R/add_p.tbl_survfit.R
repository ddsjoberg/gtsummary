#' Add p-value
#'
#' Calculate and add a p-value to stratified [`tbl_survfit()`] tables.
#'
#' @param x (`tbl_survfit`)\cr
#'   Object of class `"tbl_survfit"`
#' @param test (`string`)\cr
#'   string indicating test to use. Must be one of `"logrank"`, `"tarone"`, `"survdiff"`,
#'   `"petopeto_gehanwilcoxon"`, `"coxph_lrt"`, `"coxph_wald"`, `"coxph_score"`.
#'   See details below
#' @param test.args (named `list`)\cr
#'   named list of arguments that will be passed to the method specified in the
#'   `test` argument.
#'   Default is `NULL`.
#' @param quiet `r lifecycle::badge("deprecated")`
#' @inheritParams add_p.tbl_summary
#' @family tbl_survfit tools
#'
#' @section test argument:
#' The most common way to specify `test=` is by using a single string indicating
#' the test name. However, if you need to specify different tests within the same
#' table, the input in flexible using the list notation common throughout the
#' gtsummary package. For example, the following code would call the log-rank test,
#' and a second test of the *G-rho* family.
#' ```r
#' ... |>
#'   add_p(test = list(trt ~ "logrank", grade ~ "survdiff"),
#'         test.args = grade ~ list(rho = 0.5))
#' ```
#'
#' @export
#' @examplesIf gtsummary:::is_pkg_installed(c("survival", "broom"), reference_pkg = "gtsummary")
#' library(survival)
#'
#' gts_survfit <-
#'   list(
#'     survfit(Surv(ttdeath, death) ~ grade, trial),
#'     survfit(Surv(ttdeath, death) ~ trt, trial)
#'   ) |>
#'   tbl_survfit(times = c(12, 24))
#'
#' # Example 1 ----------------------------------
#' gts_survfit |>
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' # Pass `rho=` argument to `survdiff()`
#' gts_survfit |>
#'   add_p(test = "survdiff", test.args = list(rho = 0.5))
add_p.tbl_survfit <- function(x,
                              test = "logrank",
                              test.args = NULL,
                              pvalue_fun = label_style_pvalue(digits = 1),
                              include = everything(),
                              quiet, ...) {
  set_cli_abort_call()
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # deprecation ----------------------------------------------------------------
  if (!missing(quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::add_p(quiet)"
    )
  }

  # first identify models that are stratified ----------------------------------
  variables_strat <-
    x$cards[[1]] |>
    keep(~ !is_empty(dplyr::select(.x, cards::all_ard_groups()) |> names())) |>
    names()

  # process inputs -------------------------------------------------------------
  cards::process_selectors(
    data = scope_table_body(x$table_body)[variables_strat],
    include = {{ include }}
  )

  if (is_empty(include)) {
    cli::cli_abort(
      "There are no stratified models selected, and {.fun add_p} cannot be run.",
      call = get_cli_abort_call()
    )
  }

  if (missing(pvalue_fun)) {
    pvalue_fun <- get_theme_element("pkgwide-fn:pvalue_fun", default = pvalue_fun)
  }
  pvalue_fun <- as_function(pvalue_fun)

  test <- assign_tests(x, include, test = test)

  # saving function inputs
  rm(variables_strat)

  # add all available test meta data to a data frame ---------------------------
  df_test_meta_data <-
    imap(
      test,
      ~ dplyr::tibble(variable = .y, fun_to_run = list(.x), test_name = attr(.x, "test_name") %||% NA_character_)
    ) |>
    dplyr::bind_rows()

  # add test names to `.$table_body` so it can be used in selectors ------------
  x$table_body <-
    dplyr::left_join(
      x$table_body,
      df_test_meta_data[c("variable", "test_name")],
      by = "variable"
    ) |>
    dplyr::relocate("test_name", .after = "variable")

  # now process the `test.args` argument ---------------------------------------
  if (!missing(test.args)) {
    test.args <- inject(~!!test.args)
  }
  cards::process_formula_selectors(
    scope_table_body(x$table_body, x$inputs$data[include]),
    test.args = test.args
  )
  cards::check_list_elements(
    test.args,
    predicate = \(x) is.list(x) && is_named(x),
    error_msg = c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
                  i = "Value must be a named list.")
  )


  # calculate the results and update object to include p-value -----------------
  x <- x |>
    calculate_and_add_test_results(
      include = include,
      test.args = test.args,
      df_test_meta_data = df_test_meta_data,
      pvalue_fun = pvalue_fun,
      calling_fun = "add_p"
    )

  # update call list and return table ------------------------------------------
  x$call_list <- updated_call_list
  x
}
