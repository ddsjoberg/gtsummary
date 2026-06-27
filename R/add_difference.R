#' Add differences
#'
#' - [`add_difference.tbl_summary()`]
#' - [`add_difference.tbl_svysummary()`]
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param ... Passed to other methods.
#' @keywords internal
#'
#' @author Daniel D. Sjoberg
#' @export
add_difference <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_difference")
}


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
#' @param conf.level (`numeric`)\cr
#'   a scalar in the interval `(0, 1)` indicating the confidence level. Default is 0.95
#' @param levels (`vector`)\cr
#'   a length-two vector of the `tbl_summary(by=)` levels to compare. The
#'   difference is calculated as `levels[1]` minus `levels[2]`.
#'   This argument is required when the `by` variable has more than two levels, and
#'   allows the user to select which two groups to compare. When `by` has exactly
#'   two levels, this argument is optional and can be used to flip the direction of
#'   the difference (e.g. `levels[2]` minus `levels[1]`). Default is `NULL`.
#' @inheritParams  add_p.tbl_summary
#'
#' @export
#' @return a gtsummary table of class `"tbl_summary"`
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("broom", ref = "cardx")
#' # Example 1 ----------------------------------
#' trial |>
#'   select(trt, age, marker, response, death) %>%
#'   tbl_summary(
#'     by = trt,
#'     statistic =
#'       list(
#'         all_continuous() ~ "{mean} ({sd})",
#'         all_dichotomous() ~ "{p}%"
#'       ),
#'     missing = "no"
#'   ) |>
#'   add_n() |>
#'   add_difference()
#'
#' # Example 2 ----------------------------------
#' # ANCOVA adjusted for grade and stage
#' trial |>
#'   select(trt, age, marker, grade, stage) %>%
#'   tbl_summary(
#'     by = trt,
#'     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'     missing = "no",
#'     include = c(age, marker, trt)
#'   ) |>
#'   add_n() |>
#'   add_difference(adj.vars = c(grade, stage))
#'
#' # Example 3 ----------------------------------
#' # Select two groups to compare when `by=` has 3+ levels
#' trial |>
#'   tbl_summary(
#'     by = grade,
#'     statistic = all_continuous() ~ "{mean} ({sd})",
#'     include = c(age, marker),
#'     missing = "no"
#'   ) |>
#'   add_difference(levels = c("I", "III"))
add_difference.tbl_summary <- function(x,
                                       test = NULL,
                                       group = NULL,
                                       adj.vars = NULL,
                                       test.args = NULL,
                                       conf.level = 0.95,
                                       levels = NULL,
                                       include = everything(),
                                       pvalue_fun = label_style_pvalue(digits = 1),
                                       estimate_fun = list(
                                         c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig(),
                                         all_dichotomous() ~ label_style_sigfig(scale = 100, suffix = "%"),
                                         all_tests("smd") ~ label_style_sigfig()
                                       ),
                                       ...) {
  set_cli_abort_call()
  # check/process inputs -------------------------------------------------------
  check_dots_empty(call = get_cli_abort_call())
  updated_call_list <- c(x$call_list, list(add_difference = match.call()))

  if (rlang::is_function(estimate_fun)) {
    lifecycle::deprecate_stop(
      "1.4.0",
      "gtsummary::add_difference(estimate_fun = 'must be a list of forumulas')"
    )
  }

  # checking that input x has a by variable
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_difference} when {.code tbl_summary(by)} is not specified." |>
      cli::cli_abort(call = get_cli_abort_call())
  }

  # process/validate the `levels` argument. When supplied, the data stored in
  # `x` is subset to the two selected levels for the difference calculation; the
  # original full data is restored on the returned object below.
  original_inputs_data <- x$inputs$data
  x <- .process_difference_levels(x, levels = levels)

  # if `pvalue_fun` not modified, check if we need to use a theme p-value
  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_deprecated_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun") %||%
      pvalue_fun
  }
  pvalue_fun <- as_function(pvalue_fun)

  cards::process_selectors(
    scope_table_body(x$table_body, x$inputs$data[x$inputs$include]),
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

  cards::process_selectors(x$inputs$data, group = {{ group }}, adj.vars = {{ adj.vars }})
  check_scalar(group, allow_empty = TRUE)

  cards::process_formula_selectors(
    scope_table_body(x$table_body, x$inputs$data[include]),
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
    scope_table_body(x$table_body, x$inputs$data[include]),
    estimate_fun = estimate_fun
  )
  # fill in unspecified variables
  cards::fill_formula_selectors(
    scope_table_body(x$table_body, x$inputs$data[include]),
    estimate_fun = eval(formals(asNamespace("gtsummary")[["add_difference.tbl_summary"]])[["estimate_fun"]])
  )

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
      x = x, include = include, group = group, test.args = test.args, adj.vars = adj.vars,
      df_test_meta_data = df_test_meta_data, conf.level = conf.level,
      pvalue_fun = pvalue_fun, estimate_fun = estimate_fun, calling_fun = "add_difference"
    )

  # when `levels` is supplied, note the compared pair and direction ------------
  if (!is_empty(levels)) {
    footnote_levels <-
      glue("{translate_string('Difference')}: {levels[1]} - {levels[2]}")
    x <-
      modify_table_styling(
        x,
        columns = any_of(c("estimate", "conf.low", "conf.high", "p.value")),
        footnote = footnote_levels
      )
  }

  # restore the original full data on the returned object ----------------------
  # (the subset was only needed for the difference calculation)
  x$inputs$data <- original_inputs_data

  # update call list
  x$call_list <- updated_call_list

  # running any additional mods
  x <-
    get_theme_element("add_difference-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(x))

  x
}

#' Process the `levels` argument of `add_difference()`
#'
#' Validates the `levels` argument and, when supplied, subsets the data stored in
#' `x$inputs$data` to the two selected `by` levels and re-levels the `by` factor so
#' the difference is computed as `levels[1]` minus `levels[2]`. The full
#' `x$table_body` (all summary statistic columns) is left untouched, so the
#' resulting table keeps every group while the difference is calculated on the
#' selected pair.
#'
#' @param x (`tbl_summary`)\cr a gtsummary table
#' @param levels (`vector`)\cr the user-supplied `levels` argument (may be `NULL`)
#'
#' @return the (possibly modified) gtsummary table `x`
#' @keywords internal
#' @noRd
.process_difference_levels <- function(x, levels) {
  by_var <- x$inputs$by
  by_col <- x$inputs$data[[by_var]]

  # the distinct, non-missing levels observed in the `by` column (in level order)
  if (is.factor(by_col)) {
    by_levels <- levels(droplevels(by_col))
  } else {
    by_levels <- by_col[!is.na(by_col)] |> unique() |> sort()
  }
  n_by_levels <- length(by_levels)

  # `levels` not supplied -----------------------------------------------------
  if (is_empty(levels)) {
    if (n_by_levels != 2L) {
      cli::cli_abort(
        c(
          "Cannot run {.fun add_difference} when {.code tbl_summary(by)} column
           does not have exactly two levels.",
          i = "The {.code by} variable has {n_by_levels} levels
               ({.val {by_levels}}).",
          i = "Use the {.arg levels} argument to select the two groups to compare,
               e.g. {.code levels = c({.val {by_levels[1]}}, {.val {by_levels[2]}})}."
        ),
        call = get_cli_abort_call()
      )
    }
    # two-level default path: leave the data untouched for backward compatibility
    return(x)
  }

  # `levels` supplied: validate -----------------------------------------------
  if (length(levels) != 2L) {
    cli::cli_abort(
      c(
        "The {.arg levels} argument must be a length-two vector.",
        i = "It has length {length(levels)}."
      ),
      call = get_cli_abort_call()
    )
  }
  if (anyNA(levels) || levels[1] == levels[2]) {
    cli::cli_abort(
      "The {.arg levels} argument must contain two distinct, non-missing values.",
      call = get_cli_abort_call()
    )
  }
  not_present <- setdiff(as.character(levels), as.character(by_levels))
  if (!is_empty(not_present)) {
    cli::cli_abort(
      c(
        "Each value in the {.arg levels} argument must be one of the
         {.code tbl_summary(by)} levels.",
        i = "{cli::qty(length(not_present))} Value{?s} {.val {not_present}}
             {cli::qty(length(not_present))} {?is/are} not present.",
        i = "Valid levels are {.val {by_levels}}."
      ),
      call = get_cli_abort_call()
    )
  }

  # subset the data to the two selected levels and re-level the `by` factor so
  # the difference is computed as `levels[1]` minus `levels[2]`
  data <- x$inputs$data
  data <- data[as.character(data[[by_var]]) %in% as.character(levels), , drop = FALSE]
  data[[by_var]] <- factor(as.character(data[[by_var]]), levels = as.character(levels))

  x$inputs$data <- data

  x
}
