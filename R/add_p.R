#' Add p-values
#'
#' - [`add_p.tbl_summary()`]
#' - [`add_p.tbl_svysummary()`]
#' - [`add_p.tbl_continuous()`]
#' - [`add_p.tbl_cross()`]
#' - [`add_p.tbl_survfit()`]
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param ... Passed to other methods.
#' @keywords internal
#'
#' @author Daniel D. Sjoberg
#' @export
add_p <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_p")
}

#' Add p-values
#'
#' Adds p-values to tables created by [`tbl_summary()`] by comparing values across groups.
#'
#' @param x (`tbl_summary`)\cr
#'   table created with `tbl_summary()`
#' @param test ([`formula-list-selector`][syntax])\cr
#'   Specifies the statistical tests to perform for each variable, e.g.
#'   `list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test")`.
#'
#'   See below for details on default tests and [?tests][tests] for details on available
#'   tests and creating custom tests.
#' @param pvalue_fun (`function`)\cr
#'   Function to round and format p-values. Default is `label_style_pvalue()`.
#'   The function must have a numeric vector input, and return a string that is
#'   the rounded/formatted p-value (e.g. `pvalue_fun = label_style_pvalue(digits = 2)`).
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
#' @param adj.vars ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to include in adjusted calculations (e.g. in ANCOVA models).
#'   Default is `NULL`.
#' @inheritParams rlang::args_dots_empty
#'
#' @return a gtsummary table of class `"tbl_summary"`
#' @export
#'
#' @section test argument:
#'
#' See the [?tests][tests] help file for details on available tests and creating custom tests.
#' The [?tests][tests] help file also includes pseudo-code for each test to be clear
#' precisely how the calculation is performed.
#'
#' The default test used in `add_p()` primarily depends on these factors:
#' - whether the variable is categorical/dichotomous vs continuous
#' - number of levels in the `tbl_summary(by)` variable
#' - whether the `add_p(group)` argument is specified
#' - whether the `add_p(adj.vars)` argument is specified
#'
#' #### Specified neither `add_p(group)` nor `add_p(adj.vars)`
#'
#' - `"wilcox.test"` when `by` variable has two levels and variable is continuous.
#' - `"krustkal.test"` when `by` variable has more than two levels and variable is continuous.
#' - `"chisq.test.no.correct"` for categorical variables with all expected cell counts >=5,
#'    and `"fisher.test"` for categorical variables with any expected cell count <5.
#'
#' #### Specified `add_p(group)` and not `add_p(adj.vars)`
#'
#'  - `"lme4"` when `by` variable has two levels for all summary types.
#'
#'  *There is no default for grouped data when `by` variable has more than two levels.*
#'  *Users must create custom tests for this scenario.*
#'
#' #### Specified `add_p(adj.vars)` and not `add_p(group)`
#'
#'  - `"ancova"` when variable is continuous and `by` variable has two levels.
#'
#' @examplesIf gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' trial |>
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
                              pvalue_fun = label_style_pvalue(digits = 1),
                              group = NULL,
                              include = everything(),
                              test.args = NULL,
                              adj.vars = NULL,
                              ...) {
  set_cli_abort_call()
  # check/process inputs -------------------------------------------------------
  check_dots_empty()
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # checking that input x has a by var
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_p} when {.code tbl_summary(by)} argument not included." |>
      cli::cli_abort(call = get_cli_abort_call())
  }

  cards::process_selectors(
    scope_table_body(x$table_body, x$inputs$data[x$inputs$include]),
    include = {{ include }}
  )
  cards::process_selectors(x$inputs$data, group = {{ group }}, adj.vars = {{ adj.vars }})
  check_scalar(group, allow_empty = TRUE)

  cards::process_formula_selectors(
    scope_table_body(x$table_body, x$inputs$data[include]),
    test =
      case_switch(
        missing(test) ~ get_theme_element("add_p.tbl_summary-arg:test", default = test),
        .default = test
      ),
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

  # if `pvalue_fun` not modified, check if we need to use a theme p-value
  if (missing(pvalue_fun)) {
    pvalue_fun <-
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
      group = group,
      adj.vars = adj.vars,
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

calculate_and_add_test_results <- function(x, include, group = NULL, test.args, adj.vars = NULL,
                                           df_test_meta_data, pvalue_fun = NULL,
                                           estimate_fun = NULL, conf.level = 0.95,
                                           calling_fun, continuous_variable = NULL) {
  # list of ARDs or broom::tidy-like results
  lst_results <-
    lapply(
      include,
      \(variable) {
        # evaluate the test
        lst_captured_results <-
          cards::eval_capture_conditions(
            do.call(
              what =
                df_test_meta_data |>
                dplyr::filter(.data$variable %in% .env$variable) |>
                dplyr::pull("fun_to_run") %>%
                getElement(1),
              args = list(
                data = x$inputs[[1]], # most arg names here are data, but `tbl_survfit(x)` is a list of survfit
                variable = variable,
                by = x$inputs$by,
                group = group,
                type = tryCatch(x$inputs$type[[variable]], error = \(e) NULL), # in tbl_survfit(), the type argument is for transforming the estimate, not the summary type
                test.args = test.args[[variable]],
                adj.vars = adj.vars,
                conf.level = conf.level,
                continuous_variable = continuous_variable
              )
            )
          )

        # if there was a warning captured, print it now
        if (!is.null(lst_captured_results[["warning"]])) {
          cli::cli_inform(c(
            "The following warning was returned in {.fun {calling_fun}} for variable {.val {variable}}",
            "!" = "{lst_captured_results[['warning']]}"
          ))
        }

        # if test evaluated without error, return the result
        if (!is.null(lst_captured_results[["result"]])) return(lst_captured_results[["result"]]) # styler: off
        # otherwise, construct a {cards}-like object with error
        dplyr::tibble(
          group1 = x$inputs$by,
          variable = variable,
          stat_name = switch(calling_fun,
                             "add_p" = "p.value",
                             "add_difference" = "estimate"
          ),
          stat = list(NULL),
          warning = lst_captured_results["warning"],
          error = lst_captured_results["error"]
        ) %>%
          structure(., class = c("card", class(.)))
      }
    ) |>
    stats::setNames(include)

  # print any errors or warnings
  lst_results |>
    map(
      \(x) {
        if (inherits(x, "card")) {
          x |>
            dplyr::mutate(
              across(c(cards::all_ard_groups("levels"), cards::all_ard_variables("levels")), ~ unlist(.) |> as.character())
            )
        } else {
          NULL
        }
      }
    ) |>
    dplyr::bind_rows() %>%
    {
      switch(!is_empty(.),
             dplyr::filter(., .data$stat_name %in% c(
               "estimate", "std.error", "parameter", "statistic",
               "conf.low", "conf.high", "p.value"
             )) |>
               cards::print_ard_conditions()
      )
    }


  # combine results into a single data frame
  df_results <-
    lst_results |>
    imap(
      function(x, variable) {
        # if results are an ARD, reshape into broom::tidy-like format
        if (inherits(x, "card")) {
          res <-
            dplyr::filter(x, .data$stat_name %in% c(
              "estimate", "std.error", "parameter", "statistic",
              "conf.low", "conf.high", "p.value"
            )) |>
            cards::replace_null_statistic() |>
            tidyr::pivot_wider(
              id_cols = "variable",
              names_from = "stat_name",
              values_from = "stat"
            ) |>
            dplyr::mutate(
              across(-"variable", unlist),
              variable = .env$variable
            )
        } else {
          if (!is.data.frame(x)) {
            cli::cli_abort(
              c("Expecting the test result object for variable {.val {variable}} to be a {.cls {c('data.frame', 'tibble')}}.",
                i = "Review {.help gtsummary::tests} for details on constructing a custom function."
              ),
              call = get_cli_abort_call()
            )
          }

          res <-
            dplyr::select(x, any_of(c(
              "estimate", "std.error", "parameter", "statistic",
              "conf.low", "conf.high", "p.value"
            ))) |>
            dplyr::mutate(variable = .env$variable, .before = 1L)
          # check the result structure
          if (identical(names(res), "variable") || nrow(res) != 1L) {
            cli::cli_abort(
              c("The test result object for variable {.val {variable}} is not the expected structure.",
                i = "Review {.help gtsummary::tests} for details on constructing a custom function."
              ),
              call = get_cli_abort_call()
            )
          }
        }
        res
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::select(
      any_of(c(
        "variable", "estimate", "std.error", "parameter", "statistic",
        "conf.low", "conf.high", "p.value"
      ))
    )

  # remove new columns that already exist in gtsummary table
  new_columns <- names(df_results) |> setdiff(names(x$table_body))
  if (is_empty(new_columns)) {
    cli::cli_abort(
      c("Columns {.val {names(df_results) |> setdiff('variable')}} are already present in table (although, some may be hidden), and no new columns were added.",
        i = "Use {.code tbl |> modify_table_body(\\(x) dplyr::select(x, -p.value))} to remove columns and they will be replaced by the new columns from the current call."
      ),
      call = get_cli_abort_call()
    )
  }

  # create default footnote text
  footnote <- map(
    lst_results,
    function(x) {
      if (inherits(x, "card")) {
        ft <- x |>
          dplyr::filter(.data$stat_name %in% "method") |>
          dplyr::pull("stat") |>
          unlist()
      } else {
        ft <- x[["method"]]
      }
      ft
    }
  ) |>
    unlist() |>
    unique() |>
    translate_vector() |>
    paste(collapse = "; ")
  if (footnote == "" || is_empty(footnote)) footnote <- NULL # styler: off

  # add results to `.$table_body` ----------------------------------------------
  x <- x |>
    modify_table_body(
      ~ dplyr::left_join(
        .x,
        df_results[c("variable", new_columns)] |> dplyr::mutate(row_type = "label"),
        by = c("variable", "row_type")
      )
    )

  x <-
    modify_table_styling(
      x,
      columns = any_of(intersect("p.value", new_columns)),
      label = glue("**{translate_string('p-value')}**"),
      hide = FALSE,
      fmt_fun = pvalue_fun %||% label_style_pvalue(),
      footnote = footnote
    ) |>
    modify_table_styling(
      columns =
        intersect("estimate", new_columns),
      hide = calling_fun %in% "add_p",
      label = ifelse(is_empty(adj.vars),
                     glue("**{translate_string('Difference')}**"),
                     glue("**{translate_string('Adjusted Difference')}**")),
      footnote = footnote
    ) |>
    modify_table_styling(
      columns =
        intersect("std.error", new_columns),
      hide = TRUE,
      label = glue("**{translate_string('SE')}**"),
      fmt_fun = label_style_sigfig(digits = 3),
      footnote_abbrev = glue("**{translate_string('SE = Standard Error')}**"),
      footnote = footnote
    ) |>
    modify_table_styling(
      columns =
        intersect("parameter", new_columns),
      hide = TRUE,
      label = glue("**{translate_string('Parameter')}**"),
      fmt_fun = label_style_sigfig(digits = 3),
      footnote = footnote
    ) |>
    modify_table_styling(
      columns =
        intersect("statistic", new_columns),
      hide = TRUE,
      label = glue("**{translate_string('Statistic')}**"),
      fmt_fun = label_style_sigfig(digits = 3),
      footnote = footnote
    ) |>
    modify_table_styling(
      columns =
        intersect("conf.low", new_columns),
      hide = calling_fun %in% "add_p",
      label = glue("**{conf.level * 100}% {translate_string('CI')}**"),
      footnote = footnote,
      footnote_abbrev = glue("{translate_string('CI = Confidence Interval')}")
    )

  if (calling_fun %in% "add_difference" && all(c("conf.low", "conf.high") %in% new_columns)) {
    ci_sep <- get_theme_element("pkgwide-str:ci.sep", default = ", ")
    # use of the "ci" column was deprecated in v2.0.0
    x <- x |>
      modify_table_body(
        ~ .x |>
          dplyr::rowwise() |>
          dplyr::mutate(
            .before = "conf.low",
            ci = ifelse(
              !is.na(.data$conf.low) | !is.na(.data$conf.high),
              glue("{estimate_fun[[variable]](conf.low)}{ci_sep} {estimate_fun[[variable]](conf.high)}"),
              NA_character_
            )
          ) |>
          dplyr::ungroup()
      ) %>% # suppress deprecation warning about "ci" column
      {suppressWarnings(
        modify_header(., ci = x$table_styling$header$label[x$table_styling$header$column == "conf.low"]) |>
          modify_column_hide("ci")
      )}


    x <-
      modify_column_merge(
        x,
        pattern = paste("{conf.low}", "{conf.high}", sep = ci_sep),
        rows = !is.na(.data$conf.low)
      )
  }

  # add the specified formatting functions
  for (i in seq_along(estimate_fun)) {
    x <-
      rlang::inject(
        modify_table_styling(
          x,
          columns = any_of(c("estimate", "conf.low", "conf.high")),
          rows = .data$variable %in% !!names(estimate_fun[i]),
          fmt_fun = !!(estimate_fun[[i]] %||% label_style_sigfig())
        )
      )
  }

  # extending modify_stat_N to new columns
  x$table_styling$header <- x$table_styling$header |>
    tidyr::fill(any_of("modify_stat_N"), .direction = "downup")

  # add raw results to `.$card`
  x$cards[[calling_fun]] <- lst_results

  x
}
