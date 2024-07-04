#' Assign Test
#'
#' This function is used to assign default tests for `add_p()`
#' and `add_difference()`.
#'
#' @param x (`gtsummary`)\cr
#'   a table of class `'gtsummary'`
#' @param test (named `list`)\cr
#'   a named list of tests.
#' @param group (`string`)\cr
#'   a variable name indicating the grouping column for correlated data.
#'   Default is `NULL`.
#' @param adj.vars (`character`)\cr
#'   Variables to include in adjusted calculations (e.g. in ANCOVA models).
#' @param include (`character`)\cr
#'   Character vector of column names to assign a default tests.
#' @param calling_fun (`string`)\cr
#'   Must be one of `'add_p'` and `'add_difference'`. Depending on the context,
#'   different defaults are set.
#' @param by (`string`)\cr
#'   a single stratifying column name
#' @param cont_variable (`string`)\cr
#'   a column name of the continuous summary variable in `tbl_continuous()`
#' @param summary_type (named `list`)\cr
#'   naemd list of summary types
#' @inheritParams cli::cli_abort
#'
#' @return A table of class `'gtsummary'`
#' @name assign_tests
#'
#' @examples
#' trial |>
#'   tbl_summary(
#'     by = trt,
#'     include = c(age, stage)
#'   ) |>
#'   assign_tests(include = c("age", "stage"), calling_fun = "add_p")
NULL

#' @rdname assign_tests
#' @export
assign_tests <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("assign_tests")
}

#' @rdname assign_tests
#' @export
assign_tests.tbl_summary <- function(x,
                                     include,
                                     by = x$inputs$by,
                                     test = NULL,
                                     group = NULL,
                                     adj.vars = NULL,
                                     summary_type = x$inputs$type,
                                     calling_fun = c("add_p", "add_difference"), ...) {
  set_cli_abort_call()
  # processing inputs ----------------------------------------------------------
  calling_fun <- arg_match(calling_fun)
  data <- x$inputs$data

  # loop over the variables and assign default test if not provided by user
  lapply(
    include,
    function(variable) {
      if (is.null(test[[variable]])) {
        test[[variable]] <-
          switch(calling_fun,
                 "add_p" =
                   .add_p_tbl_summary_default_test(data,
                                                   variable = variable,
                                                   by = by, group = group, adj.vars = adj.vars,
                                                   summary_type = summary_type[[variable]]
                   ),
                 "add_difference" =
                   .add_difference_tbl_summary_default_test(data,
                                                            variable = variable,
                                                            by = by, group = group, adj.vars = adj.vars,
                                                            summary_type = summary_type[[variable]]
                   )
          )
      }

      if (is.null(test[[variable]])) {
        cli::cli_abort(
          c(
            "There is no default test set for column {.val {variable}}.",
            i = "Set a value in the {.arg test} argument for column {.val {variable}} or exclude with {.code include = -{variable}}."
          ),
          call = get_cli_abort_call()
        )
      }

      test[[variable]] <-
        .process_test_argument_value(
          test = test[[variable]],
          class = intersect(class(x), c("tbl_summary", "tbl_continuous")),
          calling_fun = calling_fun,
          variable = variable
        )
    }
  ) |>
    stats::setNames(include)
}

#' @rdname assign_tests
#' @export
assign_tests.tbl_svysummary <- function(x,
                                        include,
                                        by = x$inputs$by,
                                        test = NULL,
                                        group = NULL,
                                        adj.vars = NULL,
                                        summary_type = x$inputs$type,
                                        calling_fun = c("add_p", "add_difference"), ...) {
  set_cli_abort_call()
  # processing inputs ----------------------------------------------------------
  calling_fun <- arg_match(calling_fun)
  data <- x$inputs$data

  # all variables should already have a test assigned. This looks up the tests and converts to the function
  lapply(
    include,
    function(variable) {
      if (is.null(test[[variable]])) {
        test[[variable]] <-
          switch(calling_fun,
                 "add_difference" =
                   .add_difference_tbl_svysummary_default_test(data,
                                                               variable = variable,
                                                               by = by, group = group, adj.vars = adj.vars,
                                                               summary_type = summary_type[[variable]]
                   )
          )
      }

      if (is.null(test[[variable]])) {
        cli::cli_abort(
          c(
            "There is no default test set for column {.val {variable}}.",
            i = "Set a value in the {.arg test} argument for column {.val {variable}} or exclude with {.code include = -{variable}}."
          ),
          call = get_cli_abort_call()
        )
      }

      test[[variable]] <-
        .process_test_argument_value(
          test = test[[variable]],
          class = "tbl_svysummary",
          calling_fun = calling_fun,
          variable = variable
        )
    }
  ) |>
    stats::setNames(include)
}

#' @rdname assign_tests
#' @export
assign_tests.tbl_continuous <- function(x,
                                        include,
                                        by,
                                        cont_variable,
                                        test = NULL,
                                        group = NULL, ...) {
  set_cli_abort_call()

  # if there is a by variable (without grouping), then the default test is 'anova_2way'
  if (!is_empty(by) && is_empty(group)) {
    test <-
      rep_named(include, list("anova_2way")) |>
      utils::modifyList(val = test)
  }

  # when there is no by variable, use the same defaults as `add_p.tbl_summary`
  updated_test <-
    map(
      include,
      function(variable) {
        assign_tests.tbl_summary(x = x,
                                 include = cont_variable,
                                 by = variable,
                                 test = test[variable] |> set_names(cont_variable),
                                 group = group,
                                 adj.vars = NULL,
                                 summary_type = list("continuous") |> set_names(cont_variable),
                                 calling_fun = "add_p")[[1]]
      }
    ) |>
    set_names(include)
  return(updated_test)

  # otherwise, there is no default test.
  cli::cli_abort(
    "There is no default test for this data profile. Please specify {.arg test} argument.",
    call = get_cli_abort_call()
  )
}

#' @rdname assign_tests
#' @export
assign_tests.tbl_survfit <- function(x,
                                     include,
                                     test = NULL, ...) {
  set_cli_abort_call()

  all_possible_tests <-
    c("logrank", "tarone", "survdiff", "petopeto_gehanwilcoxon", "coxph_lrt", "coxph_wald", "coxph_score")
  if (is_string(test)) {
    test <- arg_match(test, values = all_possible_tests)
    test <- inject(everything() ~ !!test)
  }
  cards::process_formula_selectors(
    data = scope_table_body(x$table_body)[include],
    test = test
  )
  cards::fill_formula_selectors(
    data = scope_table_body(x$table_body)[include],
    test = inject(everything() ~ !!test)
  )
  cards::check_list_elements(
    x = test,
    predicate = \(x) is_string(x) && x %in% all_possible_tests,
    error_msg = "Each value passed in the {.arg test} argument must be one of {.val {all_possible_tests}}."
  )

  for (i in seq_along(test)) {
    test[[i]] <-
      .process_test_argument_value(
        test = test[[i]],
        class = "tbl_survfit",
        calling_fun = "add_p",
        variable = i
      )
  }

  test
}

.process_test_argument_value <- function(test, class, calling_fun, variable) {
  # subset the data frame
  df_tests <-
    df_add_p_tests |>
    dplyr::filter(.data$class %in% .env$class, .data[[calling_fun]])

  # if the test is character and it's an internal test
  if (is_string(test) && isTRUE(test %in% df_tests$test_name)) {
    test_to_return <- df_tests$fun_to_run[df_tests$test_name %in% test][[1]] |> eval()
    attr(test_to_return, "test_name") <- df_tests$test_name[df_tests$test_name %in% test]
    return(test_to_return)
  }

  # if the test is character and it's NOT an internal test
  if (is_string(test)) {
    test_return <- tryCatch(
      eval(parse_expr(test), envir = attr(test, ".Environment")),
      error = \(e) {
        cli::cli_abort(
          c("Could not process {.arg test} argument for variable {.val {variable}}.",
            i = "See {.help gtsummary::tests} for details on constructing custom tests. See error message below:",
            x = conditionMessage(e)),
          call = get_cli_abort_call()
        )
      }
    )
    return(test_return)
  }

  # if passed test is a function and it's an internal test
  internal_test_index <- df_tests$test_fun |>
    map_lgl(~ identical_no_attr(eval(.x), test)) |>
    which()
  if (is.function(test) && !is_empty(internal_test_index)) {
    test_to_return <- df_tests$fun_to_run[[internal_test_index]] |> eval()
    attr(test_to_return, "test_name") <- df_tests$test_name[internal_test_index]
    return(test_to_return)
  }

  # otherwise, if it's a function, return it
  test_return <-
    tryCatch(
      eval(test, envir = attr(test, ".Environment")),
      error = \(e) {
        cli::cli_abort(
          c("Could not process {.arg test} argument for variable {.val {variable}}.",
            i = "See {.help gtsummary::tests} for details on constructing custom tests. See error message below:",
            x = conditionMessage(e)),
          call = get_cli_abort_call()
        )
      }
    )

  if (!is_function(test_return)) {
    cli::cli_abort(
      c("Invalid value passed in {.arg test} argument for variable {.val {variable}}.",
        i = "Expecting a {.cls string} or {.cls function}, not {.obj_type_friendly {test_return}}."),
      call = get_cli_abort_call()
    )
  }

  test_return
}

# compare after removing attributes
identical_no_attr <- function(x, y) {
  # styler: off
  tryCatch({
    attributes(x) <- NULL
    attributes(y) <- NULL
    identical(x, y)},
    error = \(x) FALSE
  )
  # styler: on
}

.add_p_tbl_summary_default_test <- function(data, variable, by, group, adj.vars, summary_type) {
  # for continuous data, default to non-parametric tests
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% c("continuous", "continuous2") && length(unique(data[[by]])) == 2) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous_by2", default = "wilcox.test")
    return(test_func)
  }
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% c("continuous", "continuous2")) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous", default = "kruskal.test")
    return(test_func)
  }
  # now assign categorical default tests
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% c("categorical", "dichotomous")) {
    # calculate expected counts to select between chi-square and fisher
    min_exp <-
      tryCatch(
        suppressWarnings(
          table(data[[by]], data[[variable]]) |>
            proportions() %>%
            {expand.grid(rowSums(.), colSums(.))} |> #styler: off
            dplyr::mutate(
              exp = .data$Var1 * .data$Var2 *
                sum(!is.na(data[[variable]]) & !is.na(data[[by]]))
            ) %>%
            dplyr::pull(exp) |>
            min()
        ),
        error = \(e) Inf # if there is an error for whatever reason, return Inf
      )
    # if expected counts are greater than 5, then chisq test
    if (isTRUE(min_exp >= 5 || is.nan(min_exp))) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.categorical", default = "chisq.test.no.correct")
      return(test_func)
    }
    # otherwise fishers test
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.categorical.low_count", default = "fisher.test")
    return(test_func)
  }

  # now setting default tests for grouped data
  # if group variable supplied, fit a random effects model
  if (!is_empty(group) && is_empty(adj.vars) && length(unique(data[[by]])) == 2) {
    if (summary_type %in% c("continuous", "continuous2")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.continuous.group_by2", default = "lme4")
      return(test_func)
    }
    if (summary_type %in% c("categorical", "dichotomous")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.categorical.group_by2", default = "lme4")
      return(test_func)
    }
  }

  # now setting default tests for adjusted comparisons
  if (is_empty(group) && !is_empty(adj.vars) && length(unique(data[[by]])) == 2) {
    if (summary_type %in% c("continuous", "continuous2")) {
      return("ancova")
    }
  }


  return(NULL)
}


.add_difference_tbl_summary_default_test <- function(data, variable, by, group, adj.vars, summary_type) {
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% c("continuous", "continuous2")) {
    return("t.test")
  }
  if (is_empty(group) && summary_type %in% c("continuous", "continuous2")) {
    return("ancova")
  }
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% "dichotomous") {
    return("prop.test")
  }
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% "categorical") {
    return("smd")
  }

  if (!is_empty(group) && summary_type %in% c("continuous", "continuous2")) {
    return("ancova_lme4")
  }

  return(NULL)
}

.add_difference_tbl_svysummary_default_test <- function(data,
                                                        variable,
                                                        by,
                                                        group,
                                                        adj.vars,
                                                        summary_type) {
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% c("continuous", "continuous2")) {
    return("svy.t.test")
  }
  if (is_empty(group) && is_empty(adj.vars) && summary_type %in% "categorical") {
    return("smd")
  }
  # this works with and without adjustment variables
  if (is_empty(group) && summary_type %in% c("dichotomous", "continuous", "continuous2")) {
    return("emmeans")
  }

  return(NULL)
}
