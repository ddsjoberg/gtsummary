#' Add difference rows
#'
#' - [`add_difference_row.tbl_summary()`]
#'
#' @param x (`gtsummary`)\cr
#'   Object with class 'gtsummary'
#' @param ... Passed to other methods.
#' @keywords internal
#'
#' @author Daniel D. Sjoberg
#' @export
add_difference_row <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_difference_row")
}

#' Add differences rows between groups
#'
#' @description
#' `r lifecycle::badge('experimental')`\cr
#' Adds difference to tables created by [`tbl_summary()`] as additional rows.
#' This function is often useful when there are more than two groups to compare.
#'
#' Pairwise differences are calculated relative to the specified
#' `by` variable's specified reference level.
#'
#' @inheritParams add_difference.tbl_summary
#' @param reference (scalar)\cr
#'   Value of the `tbl_summary(by)` variable value that is the reference for
#'   each of the difference calculations.
#' @param header (`string`)\cr
#'   When supplied, a header row will appear above the difference statistics.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Specifies summary statistics to display for each variable.  The default is
#'   `everything() ~ "{estimate}"`. The statistics available to include will
#'   depend on the method specified in the `test` argument, but are generally
#'   `"estimate"`, `"std.error"`, `"parameter"`, `"statistic"`,
#'   `"conf.low"`, `"conf.high"`, `"p.value"`
#'
#' @export
#' @return a gtsummary table of class `"tbl_summary"`
#'
#' @details
#' The default labels for the statistic rows will often _not_ be what you need
#' to display. In cases like this, use `modify_table_body()` to directly
#' update the label rows. Use `show_header_names()` to print the underlying
#' column names to identify the columns to target when changing the label,
#' which in this case will always be the `'label'` column.
#' See Example 2.
#'
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("cardx") && gtsummary:::is_pkg_installed("broom", ref = "cardx")
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(
#'     by = grade,
#'     include = c(age, response),
#'     missing = "no",
#'     statistic = all_continuous() ~ "{mean} ({sd})"
#'   ) |>
#'   add_stat_label() |>
#'   add_difference_row(
#'     reference = "I",
#'     statistic = everything() ~ c("{estimate}", "{conf.low}, {conf.high}", "{p.value}")
#'   )
#'
#' # Example 2 ----------------------------------
#' # Function to build age-adjusted logistic regression and put results in ARD format
#' ard_odds_ratio <- \(data, variable, by, ...) {
#'   cardx::construct_model(
#'     data = data,
#'     formula = reformulate(response = variable, termlabels = c(by, "age")), # adjusting model for age
#'     method = "glm",
#'     method.args = list(family = binomial)
#'   ) |>
#'     cardx::ard_regression_basic(exponentiate = TRUE) |>
#'     dplyr::filter(.data$variable == .env$by)
#' }
#'
#' trial |>
#'   tbl_summary(by = trt, include = response, missing = "no") |>
#'   add_stat_label() |>
#'   add_difference_row(
#'     reference = "Drug A",
#'     statistic = everything() ~ c("{estimate}", "{conf.low}, {conf.high}", "{p.value}"),
#'     test = everything() ~ ard_odds_ratio,
#'     estimate_fun = everything() ~ label_style_ratio()
#'   ) |>
#'   # change the default label for the 'Odds Ratio'
#'   modify_table_body(
#'     ~ .x |>
#'       dplyr::mutate(
#'         label = ifelse(label == "Coefficient", "Odds Ratio", label)
#'       )
#'   ) |>
#'   # add footnote about logistic regression
#'   modify_footnote_body(
#'     footnote = "Age-adjusted logistic regression model",
#'     column = "label",
#'     rows = variable == "response-row_difference"
#'   )
add_difference_row.tbl_summary <- function(x,
                                           reference,
                                           statistic = everything() ~ "{estimate}",
                                           test = NULL,
                                           group = NULL,
                                           header = NULL,
                                           adj.vars = NULL,
                                           test.args = NULL,
                                           conf.level = 0.95,
                                           include = everything(),
                                           pvalue_fun = label_style_pvalue(digits = 1),
                                           estimate_fun = list(
                                             c(all_continuous(), all_categorical(FALSE)) ~ label_style_sigfig(),
                                             all_dichotomous() ~ label_style_sigfig(scale = 100, suffix = "%"),
                                             all_tests("smd") ~ label_style_sigfig()
                                           ),
                                           ...) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  updated_call_list <- c(x$call_list, list(add_difference = match.call()))
  check_not_missing(reference)
  check_scalar(reference)

  # checking that input x has a by var and it has two levels
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_difference_row} when {.code tbl_summary()} does not include a {.arg by} argument." |>
      cli::cli_abort(call = get_cli_abort_call())
  }

  # if `pvalue_fun` not modified, check if we need to use a theme p-value
  if (missing(pvalue_fun)) {
    pvalue_fun <-
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
      "The {.code add_difference_row()} results for categorical variables may not
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

  cards::process_formula_selectors(
    scope_table_body(x$table_body, x$inputs$data[include]),
    statistic = statistic
  )
  cards::check_list_elements(
    statistic,
    predicate = \(x) is.character(x) && !is_empty(.extract_glue_elements(x)),
    error_msg =
      c("Each element passed in the {.arg statistic} argument must be a character vector with at least one glue element.",
        "i" = "For example, {.code everything() ~ c('{{estimate}}', '{{conf.low}}, {{conf.high}}', '{{p.value}}')}"),
  )

  # select test ----------------------------------------------------------------
  test <-
    assign_tests(
      x = x,
      test = test,
      group = group,
      adj.vars = adj.vars,
      include = include,
      calling_fun = "add_difference" # this gives us the defaults we want
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

  # check reference level is appropriate
  lst_by_levels <-
    x$table_styling$header |>
    dplyr::filter(grepl(pattern = "^stat_\\d*[1-9]\\d*$", x = .data$column)) |>
    dplyr::select("column", "modify_stat_level") |>
    deframe() |>
    lapply(FUN = as.character)
  if (!as.character(reference) %in% unlist(lst_by_levels)) {
    cli::cli_abort(
      "The {.arg reference} argument must be one of {.val {unlist(lst_by_levels)}}.",
      call = get_cli_abort_call()
    )
  }

  # prep data for tests, by adding reference level to the first position in factor
  data <- x$inputs$data
  data[[x$inputs$by]] <- fct_relevel(data[[x$inputs$by]], reference, after = 0L)

  # create data frame that is one line per test to be calculated
  df_results <-
    tidyr::expand_grid(
      variable = include,
      reference_level = reference,
      compare_level = unlist(lst_by_levels) |> setdiff(reference)
    ) |>
    # merge in table_body column name and subsetted data frame
    dplyr::left_join(
      enframe(unlist(lst_by_levels), "column", "compare_level") |>
        dplyr::filter(!.data$compare_level %in% .env$reference) |>
        dplyr::mutate(
          data =
            map(
              .data$compare_level,
              \(.x, .y) {
                .env$data |>
                  dplyr::filter(.data[[x$inputs$by]] %in% c(.x, .env$reference)) |>
                  dplyr::mutate("{x$inputs$by}" := factor(.data[[x$inputs$by]])) # this removes unobserved levels of a factor
              }
            )
        ),
      by = "compare_level"
    )

  df_results$result <-
    pmap(
      list(df_results$variable,
           df_results$data),
      \(variable, data) {
        .calculate_one_test(
          data = data,
          variable = variable,
          x = x,
          df_test_meta_data = df_test_meta_data,
          estimate_fun = estimate_fun,
          pvalue_fun = pvalue_fun,
          group = group,
          test.args = test.args,
          adj.vars = adj.vars,
          conf.level = conf.level,
          apply_fmt_fn = TRUE
        )
      }
    )

  # create vector of results
  df_results$result_fmt <-
    pmap(
      list(df_results$variable, df_results$result),
      \(variable, result) {
        lst_results <-
          result |>
          dplyr::filter(map_lgl(.data$stat_fmt, Negate(is.null))) |>
          cards::get_ard_statistics(.column = "stat_fmt")

        map(
          statistic[[variable]],
          ~ glue::glue_data(.x = lst_results, .x)
        )
      }
    )

  # create label for new statistics
  df_results$result_lbl <-
    pmap(
      list(df_results$variable, df_results$result),
      \(variable, result) {
        lst_results <-
          result |>
          dplyr::filter(map_lgl(.data$stat_fmt, Negate(is.null))) |>
          cards::get_ard_statistics(.column = "stat_label")

        map(
          statistic[[variable]],
          ~ ifelse(
            .x == "{conf.low}, {conf.high}",
            glue::glue("{style_number(conf.level, scale = 100)}% CI"), # replace {conf.low}, {conf.high} with "95% CI"
            glue::glue_data(.x = lst_results, .x)
          )
        )
      }
    )

  # prep results to place them in table_body
  df_results_wide <-
    df_results |>
    dplyr::left_join(
      df_test_meta_data[c("variable", "test_name")],
      by = "variable"
    ) |>
    dplyr::select("variable", "test_name", "column", "result_fmt", label = "result_lbl") |>
    tidyr::unnest(cols = c("result_fmt", "label")) |>
    dplyr::mutate(across(c("result_fmt", "label"), unlist)) |>
    tidyr::pivot_wider(
      id_cols = c("variable", "test_name", "label"),
      values_from = "result_fmt",
      names_from = "column"
    ) |>
    dplyr::mutate(row_type = "difference_row")

  # get index values where new lines are to be inserted
  variable_index <-
    x$table_body |>
    dplyr::select("variable") |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    dplyr::filter(
      .by = "variable",
      dplyr::n() == dplyr::row_number(),
      .data$variable %in% .env$include
    ) |>
    dplyr::slice_tail(by = "variable", n = 1L) |>
    deframe()

  # add each of the rows to table_body
  for (v in rev(names(variable_index))) {
    x$table_body <-
      x$table_body |>
      dplyr::add_row(
        dplyr::bind_rows(
          if (!is_empty(.env$header)) {
            data.frame(variable = paste0(v, "-row_difference"), row_type = "label", label = header)
          },
          dplyr::filter(df_results_wide, .data$variable == .env$v) |>
            dplyr::mutate(variable = paste0(.data$variable, "-row_difference"))
        ),
        .after = variable_index[[v]]
      )
  }

  # prepping ARD to return with result -----------------------------------------
  card <-
    df_results |>
    dplyr::rowwise() |>
    dplyr::mutate(
      result_lst =
        list(.data$result) |>
        set_names(nm = paste(shQuote(.data$reference_level, type = "sh"), shQuote(.data$compare_level, type = "sh"), sep = " vs. "))
    ) |>
    dplyr::select("variable", "result_lst") |>
    tidyr::nest(result_nested = "result_lst") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      result_final =
        list(.data$result_nested[[1]]) |>
        set_names(nm = .data$variable)

    ) |>
    dplyr::pull("result_final")

  # add info to table ----------------------------------------------------------
  x$call_list[["add_difference_row"]] <- match.call()
  x$cards[["add_difference_row"]] <- card
  # print warnings/errors from calculations
  x$cards[["add_difference_row"]] |>
    map(dplyr::bind_rows) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$stat_name %in% c("estimate", "std.error", "parameter",
                                         "statistic", "conf.low", "conf.high", "p.value")) |>
    cards::print_ard_conditions()

  # add final styling to table -------------------------------------------------
  x |>
    .modify_indent(
      columns = "label",
      rows = .data$row_type == "difference_row",
      indent = 4L
    ) |>
    .modify_missing_symbol(
      columns =
        x$table_styling$header |>
        dplyr::filter(.data$modify_stat_level == .env$reference) |>
        dplyr::pull("column"),
      rows = .data$row_type == "difference_row",
      symbol = "\U2014"
    )
}
