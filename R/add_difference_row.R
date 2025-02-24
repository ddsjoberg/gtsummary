
add_difference_row <- function(x, ...) {
  check_not_missing(x)
  check_class(x, "gtsummary")
  UseMethod("add_difference_row")
}


add_difference_row.tbl_summary <- function(x,
                                           reference,
                                           statistic,
                                           test = NULL,
                                           group = NULL,
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
  # check intputs --------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  updated_call_list <- c(x$call_list, list(add_difference = match.call()))
  check_not_missing(reference)
  check_not_missing(statistic)
  check_scalar(reference)

  # checking that input x has a by var and it has two levels
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_difference_row} when {.code tbl_summary()} does not include a {.arg by} argument." |>
      cli::cli_abort(call = get_cli_abort_call())
  }

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

  # TODO: Assign a default test
  test = rep_named(include, list("t.test"))

  # check reference level is appropriate
  lst_by_levels <-
    x$table_styling$header |>
    dplyr::filter(startsWith(.data$column, "stat_")) |>
    dplyr::select("column", "modify_stat_level") |>
    deframe() |>
    as.list()
  if (!as.character(reference) %in% unlist(lst_by_levels)) {
    cli::cli_abort(
      "The {.arg reference} argument must be one of {.val {unlist(lst_by_levels)}}.",
      call = get_cli_abort_call()
    )
  }

  # prep data for tests
  data[[x$inputs$by]] <- forcats::fct_relevel(data[[x$inputs$by]], reference, after = 0L)
}
