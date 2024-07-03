#' Add p-values
#'
#' Adds p-values to tables created by [`tbl_svysummary()`] by comparing values across groups.
#'
#' @param x (`tbl_svysummary`)\cr
#'   table created with `tbl_svysummary()`
#' @param test ([`formula-list-selector`][syntax])\cr
#'   List of formulas specifying statistical tests to perform.
#'   Default is `list(all_continuous() ~ "svy.wilcox.test", all_categorical() ~ "svy.chisq.test")`.
#'
#'   See below for details on default tests and [?tests][tests] for details on available
#'   tests and creating custom tests.
#' @inheritParams add_p.tbl_summary
#' @inheritParams rlang::args_dots_empty
#'
#' @return a gtsummary table of class `"tbl_svysummary"`
#' @export
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("cardx", "survey"), reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")
#' # Example 1 ----------------------------------
#' # A simple weighted dataset
#' survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
#'   tbl_svysummary(by = Survived, include = c(Sex, Age)) |>
#'   add_p()
#'
#' # A dataset with a complex design
#' data(api, package = "survey")
#' d_clust <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#'
#' # Example 2 ----------------------------------
#' tbl_svysummary(d_clust, by = both, include = c(api00, api99)) |>
#'   add_p()
#'
#' # Example 3 ----------------------------------
#' # change tests to svy t-test and Wald test
#' tbl_svysummary(d_clust, by = both, include = c(api00, api99, stype)) |>
#'   add_p(
#'     test = list(
#'       all_continuous() ~ "svy.t.test",
#'       all_categorical() ~ "svy.wald.test"
#'     )
#'   )
add_p.tbl_svysummary <- function(x,
                              test = list(all_continuous() ~ "svy.wilcox.test",
                                          all_categorical() ~ "svy.chisq.test"),
                              pvalue_fun = label_style_pvalue(digits = 1),
                              include = everything(),
                              test.args = NULL,
                              ...) {
  set_cli_abort_call()
  check_pkg_installed("survey", reference_pkg = "gtsummary")

  # check/process inputs -------------------------------------------------------
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # checking that input x has a by var
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_p} when {.code tbl_svysummary(by)} argument not included." |>
      cli::cli_abort(call = get_cli_abort_call())
  }

  cards::process_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[x$inputs$include]),
    include = {{ include }}
  )

  cards::process_formula_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[include]),
    test =
      case_switch(
        missing(test) ~ get_theme_element("add_p.tbl_svysummary-arg:test", default = test),
        .default = test
      ),
    include_env = TRUE
  )

  cards::fill_formula_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[include]),
    test = eval(formals(asNamespace("gtsummary")[["add_p.tbl_svysummary"]])[["test"]])
  )
  # add the calling env to the test
  test <- .add_env_to_list_elements(test, env = caller_env())

  cards::check_list_elements(
    test,
    predicate = \(x) is.character(x) || is.function(x),
    error_msg = c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
                  i = "Value must be {.cls character} or {.cls function}."
    )
  )

  # if `pvalue_fun` not modified, check if we need to use a theme p-value
  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_deprecated_theme_element("add_p.tbl_svysummary-arg:pvalue_fun") %||%
      get_deprecated_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun") %||%
      pvalue_fun
  }
  pvalue_fun <- as_function(pvalue_fun)

  # select test ----------------------------------------------------------------
  test <-
    assign_tests(
      x = x,
      test = test,
      include = include,
      calling_fun = "add_p"
    )

  # add all available test meta data to a data frame ---------------------------
  df_test_meta_data <-
    imap(
      test,
      ~ dplyr::tibble(variable = .y, fun_to_run = list(.x), test_name = attr(.x, "test_name") %||% NA_character_)
    ) |>
    dplyr::bind_rows()

  # add test names to `.$table_body` so it can be used in selectors ------------
  if (!"test_name" %in% names(x$table_body)) {
    x$table_body <-
      dplyr::left_join(
        x$table_body,
        df_test_meta_data[c("variable", "test_name")],
        by = "variable"
      ) |>
      dplyr::relocate("test_name", .after = "variable")
  } else {
    x$table_body <-
      dplyr::rows_update(
        x$table_body,
        df_test_meta_data[c("variable", "test_name")],
        by = "variable",
        unmatched = "ignore"
      ) |>
      dplyr::relocate("test_name", .after = "variable")
  }


  # now process the `test.args` argument ---------------------------------------
  cards::process_formula_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[include]),
    test.args = test.args
  )
  cards::check_list_elements(
    test.args,
    predicate = \(x) is.list(x) && is_named(x),
    error_msg = c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
                  i = "Value must be a named list."
    )
  )

  # calculate tests ------------------------------------------------------------
  x <-
    calculate_and_add_test_results(
      x = x, include = include, group = NULL, test.args = test.args, adj.vars = NULL,
      df_test_meta_data = df_test_meta_data, pvalue_fun = pvalue_fun, calling_fun = "add_p"
    )

  # update call list
  x$call_list <- updated_call_list

  # running any additional mods
  x <-
    get_theme_element("add_p-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}
