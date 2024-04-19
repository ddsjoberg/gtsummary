#' Adds P-value
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param ... Passed to other methods.
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @export
#'
#' @seealso [`add_p.tbl_summary()`]
add_p <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_p")
}

#' Add p-values to summary table
#'
#' Adds p-values to tables created by [`tbl_summary()`] by comparing values across groups.
#'
#' @param x (`tbl_summary`)\cr
#'   table created with `tbl_summary()`
#' @param test ([`formula-list-selector`][syntax])\cr
#'   Specifies the statistical tests to perform for each variable, e.g. `
#'   list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test")`.
#'   Common tests include `"t.test"`, `"aov"`, `"wilcox.test"`, `"kruskal.test"`,
#'   `"chisq.test"`, `"fisher.test"`, and `"lme4"` (for clustered data).
#'   See [tests] for details, more tests, and instruction for implementing a custom test.
#'
#'   Default tests when `group` argument not specified are `"kruskal.test"`
#'   for continuous variables (`"wilcox.test"` when "by" variable has two levels),
#'   `"chisq.test.no.correct"` for categorical variables with all expected cell
#'   counts >=5, and `"fisher.test"` for categorical variables with any expected cell count <5.
#' @param pvalue_fun (`function`)\cr
#'   Function to round and format p-values. Default is `styfn_pvalue()`.
#'   The function must have a numeric vector input, and return a string that is
#'   the rounded/formatted p-value (e.g. `pvalue_fun = styfn_pvalue(digits = 2)`).
#' @param group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable name of an ID or grouping variable. The column can be used to
#'   calculate p-values with correlated data.
#'   Default is `NULL`. See [tests] for methods that utilize the `group` argument.
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to include in output. Default is `everything()`.
#' @param test.args ([`formula-list-selector`][syntax])\cr
#'   Containing additional arguments to pass to tests that accept arguments.
#'   For example, add an argument for all t-tests, use
#'   `test.args = all_tests("t.test") ~ list(var.equal = TRUE)`.
#' @param ... Not used
#'
#' @return a gtsummary table of class `"tbl_summary"`
#' @export
#'
#' @examplesIf gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")
#' # Example 1 ----------------------------------
#' add_p_ex1 <-
#'   trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' add_p_ex2 <-
#'   trial %>%
#'   select(trt, age, marker) |>
#'   tbl_summary(by = trt, missing = "no") |>
#'   add_p(
#'     # perform t-test for all variables
#'     test = everything() ~ "t.test",
#'     # assume equal variance in the t-test
#'     test.args = all_tests("t.test") ~ list(var.equal = TRUE)
#'   )
add_p.tbl_summary <- function(x,
                              test = NULL,
                              pvalue_fun = styfn_pvalue(),
                              group = NULL,
                              include = everything(),
                              test.args = NULL,
                              ...) {
  set_cli_abort_call()
  # check/process inputs -------------------------------------------------------
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # checking that input x has a by var
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_p} when {.code tbl_summary(by)} argument not included." |>
      cli::cli_abort()
  }

  cards::process_selectors(
    select_prep(x$table_body, x$inputs$data[x$inputs$include]),
    include = {{ include }}
  )
  cards::process_selectors(x$inputs$data, group = {{ group }})
  check_scalar(group, allow_empty = TRUE)

  cards::process_formula_selectors(
    select_prep(x$table_body, x$inputs$data[include]),
    test = test,
    include_env = TRUE
  )
  # add the calling env to the test
  test <- .add_env_to_list_elements(test, env = caller_env())


  cards::check_list_elements(
    test,
    predicate = \(x) is.character(x) || is.function(x),
    error_msg = c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
                  i = "Value must be {.cls character} or {.cls function}.")
  )

  # if `pvalue_fun` not modified, check if we need to use a theme p-value
  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun") %||%
      pvalue_fun
  }
  pvalue_fun <- as_function(pvalue_fun)

  # select test ----------------------------------------------------------------
  test <-
    assign_tests(
      x = x,
      test = test,
      group = group,
      include = include,
      calling_fun = "add_p"
    )

  # add all available test meta data to a data frame ---------------------------
  df_test_meta_data <- .test_meta_data(test)

  # add test names to `.$table_body` so it can be used in selectors ------------
  x$table_body <-
    dplyr::left_join(
      x$table_body,
      df_test_meta_data[c("variable", "test_name")],
      by = "variable"
    ) |>
    dplyr::relocate("test_name", .after = "variable")

  # now process the `test.args` argument ---------------------------------------
  cards::process_formula_selectors(
    select_prep(x$table_body, x$inputs$data[include]),
    test.args = test.args
  )
  cards::check_list_elements(
    test.args,
    predicate = \(x) is.list(x) && is_named(x),
    error_msg = c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
                  i = "Value must be a named list.")
  )

  # calculate p-values ---------------------------------------------------------
  # list of ARDs or broom::tidy-like results
  lst_results <-
    lapply(
      include,
      \(variable) {
        do.call(
          what =
            df_test_meta_data |>
            dplyr::filter(.data$variable %in% .env$variable) |>
            dplyr::pull("fun_to_run") %>%
            getElement(1) |>
            eval(),
          args = list(
            data = x$inputs$data,
            variable = variable,
            by = x$inputs$by,
            group = group,
            type = x$inputs$type[[variable]],
            test.args = test.args[[variable]]
          )
        )
      }
    ) |>
    stats::setNames(include)

  # combine results into a single data frame
  df_results <-
    lst_results |>
    imap(
      function(x, variable) {
        # if results are an ARD, reshape into broom::tidy-like format
        if (inherits(x, "card")) {
          res <-
            dplyr::filter(x, .data$stat_name %in% c("estimate", "parameter", "statistic",
                                                    "conf.low", "conf.high", "p.value")) |>
            tidyr::pivot_wider(
              id_cols = "variable",
              names_from = "stat_name",
              values_from = "stat"
            ) |>
            dplyr::mutate(across(-"variable", unlist))
        }
        else {
          res <-
            dplyr::select(x, any_of(c("estimate", "parameter", "statistic",
                                      "conf.low", "conf.high", "p.value"))) |>
            dplyr::mutate(variable = .env$variable, .before = 1L)
        }
        res
      }
    ) |>
    dplyr::bind_rows()

  # create default footnote text
  footnote <- map(
    lst_results,
    function(x) {
      if (inherits(x, "card")) {
        ft <- x |>
          dplyr::filter(.data$stat_name %in% "method") |>
          dplyr::pull("stat") |>
          unlist()
      }
      else  {
        ft <- x[["method"]]
      }
      ft
    }
  ) |>
    unlist() |>
    paste(collapse = "; ")

  # add results to `.$table_body` ----------------------------------------------
  x <- x |>
    modify_table_body(
      ~dplyr::left_join(
        .x,
        df_results |> dplyr::mutate(row_type = "header"),
        by = c("variable", "row_type")
    )
    ) |>
    modify_table_styling(
      columns = "p.value",
      label = "**p-value**",
      hide = FALSE,
      fmt_fun = styfn_pvalue(),
      footnote = footnote
    )

  # adding labels for hidden columns
  x$table_styling$header <- x$table_styling$header |>
    dplyr::mutate(
      label = ifelse(.data$column %in% "statistic", "**Statistic**", .data$label)
    ) |>
    tidyr::fill("modify_stat_N", .direction = "downup") # fill missing N for new cols



  # add raw results to `.$card`
  x$cards$add_p <- lst_results

  # update call list
  x$call_list <- updated_call_list

  x
}

# TODO: add the data set of tests to the package so we can assign the name
#       perhaps we can allow test names to be attributes of user-defined
#       tests, so that custom tests can also be used in selectors?
.test_meta_data <- function(test) {
  imap(
    test,
    function(passed_test, variable) {
      # this selects tests that ship with the package
      df_test_meta_data <-
        df_add_p_tests |>
        dplyr::filter(.data$class %in% "tbl_summary", .data$add_p) |>
        dplyr::filter(
          # styler: off
          if (is.character(.env$passed_test)) .data$test_name %in% .env$passed_test
          else map_lgl(.data$test_fun, ~tryCatch(identical(.x, .env$passed_test), error = \(x) FALSE))
          # styler: on
        ) |>
        dplyr::select("test_name", "fun_to_run", "accept_dots") |>
        dplyr::mutate(variable = .env$variable, .before = 1L)

      # TODO: Add support for user-defined tests having an associted name (via an attr?)
      if (nrow(df_test_meta_data) == 0L) {
        df_test_meta_data <-
          dplyr::tibble(
            variable = .env$variable,
            test_name = NA_character_,
            fun_to_run =
              # styler: off
              if (is.function(passed_test)) passed_test |> list()
              else {
                rlang::parse_expr(passed_test) |>
                  eval_tidy(env = attr(passed_test, ".Environment")) |>
                  structure(.Environment = attr(passed_test, ".Environment")) |>
                  list()
              },
              # styler: on
            accept_dots = FALSE # TODO: is this true? in the last version of gtsummary, did user-defined functions accept dots?
          )
      }

      df_test_meta_data
    }
  ) |>
    dplyr::bind_rows()
}


