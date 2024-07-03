#' Add p-values
#'
#' @inheritParams add_p.tbl_summary
#' @param x (`tbl_continuous`)\cr
#'   table created with `tbl_continuous()`
#' @param test List of formulas specifying statistical tests to perform for each
#' variable.
#' Default is two-way ANOVA when `by=` is not `NULL`, and has the same defaults
#' as `add_p.tbl_continuous()` when `by = NULL`.
#' See [tests] for details, more tests, and instruction for implementing a custom test.
#' @export
#' @return 'tbl_continuous' object
#'
#' @examplesIf gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary")
#' trial |>
#'   tbl_continuous(variable = age, by = trt, include = grade) |>
#'   add_p()
add_p.tbl_continuous <- function(x,
                                 test = NULL,
                                 pvalue_fun = label_style_pvalue(digits = 1),
                                 include = everything(),
                                 test.args = NULL,
                                 group = NULL, ...) {
  set_cli_abort_call()

  # check/process inputs -------------------------------------------------------
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  cards::process_selectors(
    scope_table_body(x$table_body, x$inputs$data[x$inputs$include]),
    include = {{ include }}
  )

  cards::process_selectors(
    x$inputs$data,
    group = {{ group }}
  )

  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_theme_element("pkgwide-fn:pvalue_fun", default = pvalue_fun)
  }
  pvalue_fun <- as_function(pvalue_fun)

  cards::process_formula_selectors(
    scope_table_body(x$table_body, x$inputs$data[include]),
    test = test,
    include_env = TRUE
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

  # select test ----------------------------------------------------------------
  test <-
    assign_tests(
      x = x,
      test = test,
      group = group,
      adj.vars = NULL,
      include = include,
      by = x$inputs$by,
      cont_variable = x$inputs$variable,
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
    scope_table_body(x$table_body, x$inputs$data[include]),
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
      x = x, include = include, group = group, test.args = test.args, adj.vars = NULL,
      df_test_meta_data = df_test_meta_data, pvalue_fun = pvalue_fun, calling_fun = "add_p",
      continuous_variable = x$inputs$variable
    )

  # update call list
  x$call_list <- updated_call_list

  x
}
