#' Add differences between groups
#'
#' Adds difference to tables created by [`tbl_summary()`].
#' The difference between two groups (typically mean or rate difference) is added
#' to the table along with the difference's confidence interval and a p-value (when applicable).
#'
#' @param x (`tbl_summary`)\cr
#'   table created with `tbl_summary()`
#' @param test ([`formula-list-selector`][syntax])\cr
#'   Specifies the tests/methods to perform for each variable, e.g.
#'   `list(all_continuous() ~ "t.test", all_dichotomous() ~ "prop.test", all_categorical(FALSE) ~ "smd")`.
#'
#'   See below for details on default tests and [?tests][tests] for details on available
#'   tests and creating custom tests.
#' @param estimate_fun ([`formula-list-selector`][syntax])\cr
#'   List of formulas specifying the functions
#'   to round and format differences and confidence limits.
#'   Default is
#'   `list(c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig(), all_categorical() ~ \(x) paste0(style_sigfig(x, scale = 100), "%"))`
#' @param conf.level (`numeric`)\cr
#'   a scalar in the interval `(0, 1)` indicating the confidence level. Default is 0.95
#' @inheritParams  add_p.tbl_summary
#'
#' @export
#' @return a gtsummary table of class `"tbl_summary"`
#'
#' @examplesIf gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")
add_difference.tbl_svysummary <- function(x,
                                          test = NULL,
                                          group = NULL,
                                          adj.vars = NULL,
                                          test.args = NULL,
                                          conf.level = 0.95,
                                          include = everything(),
                                          pvalue_fun = label_style_pvalue(digits = 1),
                                          estimate_fun = list(
                                            c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig(),
                                            all_dichotomous() ~ function(x) ifelse(is.na(x), NA_character_, paste0(style_sigfig(x, scale = 100), "%")),
                                            all_tests("smd") ~ label_style_sigfig()
                                          ),
                                          ...) {
  set_cli_abort_call()
  # check/process inputs -------------------------------------------------------
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_difference = match.call()))

  if (rlang::is_function(estimate_fun)) {
    lifecycle::deprecate_stop(
      "1.4.0",
      "gtsummary::add_difference(estimate_fun = 'must be a list of forumulas')"
    )
  }

  # checking that input x has a by var and it has two levels
  if (is_empty(x$inputs$by) || dplyr::n_distinct(as.data.frame(x$inputs$data)[[x$inputs$by]], na.rm = TRUE) != 2L) {
    "Cannot run {.fun add_difference} when {.code tbl_summary(by)} column does not have exactly two levels." |>
      cli::cli_abort(call = get_cli_abort_call())
  }

  # if `pvalue_fun` not modified, check if we need to use a theme p-value
  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_deprecated_theme_element("add_p.tbl_svysummary-arg:pvalue_fun") %||%
      get_deprecated_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun") %||%
      pvalue_fun
  }
  pvalue_fun <- as_function(pvalue_fun)

  cards::process_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[x$inputs$include]),
    include = {{ include }}
  )

  # checking for `tbl_summary(percent = c("cell", "row"))`, which don't apply
  if (!x$inputs$percent %in% "column" &&
      any(unlist(x$inputs$type[include]) %in% c("categorical", "dichotomous"))) {
    cli::cli_warn(c(
      "The {.code add_difference()} results for categorical variables may not
       compatible with {.code tbl_summary(percent = c('cell', 'row'))}.",
      i = "Use column percentages instead, {.code tbl_summary(percent = 'column')}."
    ))
  }

  cards::process_selectors(as.data.frame(x$inputs$data), group = {{ group }}, adj.vars = {{ adj.vars }})
  check_scalar(group, allow_empty = TRUE)

  cards::process_formula_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[include]),
    test = test,
    include_env = TRUE
  )
  # add the calling env to the test
  test <- .add_env_to_list_elements(test, env = caller_env())

  # select test ----------------------------------------------------------------
  test <-
    assign_tests(
      x = x,
      test = test,
      group = group,
      adj.vars = adj.vars,
      include = include,
      calling_fun = "add_difference"
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

  # now process the `test.args` and `estimate_fun` arguments -------------------
  cards::process_formula_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[include]),
    estimate_fun = estimate_fun
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(x$table_body, as.data.frame(x$inputs$data)[include]),
    estimate_fun = eval(formals(asNamespace("gtsummary")[["add_difference.tbl_svysummary"]])[["estimate_fun"]])
  )

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
      x = x, include = include, group = group, test.args = test.args, adj.vars = adj.vars,
      df_test_meta_data = df_test_meta_data, conf.level = conf.level,
      pvalue_fun = pvalue_fun, estimate_fun = estimate_fun, calling_fun = "add_difference"
    )

  # update call list
  x$call_list <- updated_call_list

  # running any additional mods
  x <-
    get_theme_element("add_difference-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}
