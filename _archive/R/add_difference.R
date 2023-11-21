#' Add difference between groups
#'
#' Add the difference between two groups (typically mean difference),
#' along with the difference confidence interval and p-value.
#'
#' @param x `"tbl_summary"` or `"tbl_svysummary"` object
#' @inheritParams  add_p.tbl_summary
#' @inheritParams tbl_regression
#' @param adj.vars Variables to include in mean difference adjustment (e.g. in ANCOVA models)
#' @param estimate_fun List of formulas specifying the formatting functions
#' to round and format differences. Default is
#' `list(all_continuous() ~ style_sigfig, all_categorical() ~ function(x) paste0(style_sigfig(x * 100), "%"))`
#' Function to round and format difference. Default is [style_sigfig()]
#' @param test List of formulas specifying statistical tests to perform for each variable,
#' e.g. `list(all_continuous() ~ "t.test")`.
#' Common tests include `"t.test"` or `"ancova"` for continuous data, and
#' `"prop.test"` for dichotomous variables.
#' See [tests] for details and more tests.
#' @export
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#'
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' add_difference_ex1 <-
#'   trial %>%
#'   select(trt, age, marker, response, death) %>%
#'   tbl_summary(
#'     by = trt,
#'     statistic =
#'       list(
#'         all_continuous() ~ "{mean} ({sd})",
#'         all_dichotomous() ~ "{p}%"
#'       ),
#'     missing = "no"
#'   ) %>%
#'   add_n() %>%
#'   add_difference()
#'
#' # Example 2 ----------------------------------
#' # ANCOVA adjusted for grade and stage
#' add_difference_ex2 <-
#'   trial %>%
#'   select(trt, age, marker, grade, stage) %>%
#'   tbl_summary(
#'     by = trt,
#'     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'     missing = "no",
#'     include = c(age, marker, trt)
#'   ) %>%
#'   add_n() %>%
#'   add_difference(adj.vars = c(grade, stage))
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_difference_ex1.png", width = "60")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_difference_ex2.png", width = "60")`
#' }}
add_difference <- function(x, test = NULL, group = NULL,
                           adj.vars = NULL, test.args = NULL,
                           conf.level = 0.95, include = everything(),
                           pvalue_fun = NULL, estimate_fun = NULL) {
  # checking inputs ------------------------------------------------------------
  updated_call_list <- c(x$call_list, list(add_difference = match.call()))
  .assert_class(x, c("tbl_summary", "tbl_svysummary"))

  if (is.null(x$by) || nrow(x$df_by) != 2) {
    stop("'tbl_summary'/'tbl_svysummary' object must have a `by=` value with exactly two levels", call. = FALSE)
  }
  if (any(c("add_p", "add_difference") %in% names(x$call_list))) {
    stop("`add_difference()` cannot be run after `add_p()` or `add_difference()`", call. = FALSE)
  }
  if (rlang::is_function(estimate_fun)) {
    lifecycle::deprecate_stop(
      "1.4.0",
      "gtsummary::add_difference(estimate_fun = 'must be a list of forumulas')",
      details = "Argument has been converted to `list(everything() ~ estimate_fun)`"
    )
  }

  # expanding formula lists/var selects ----------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      data = select(.extract_data_frame(x$inputs$data), any_of(x$meta_data$variable)),
      var_info = x$table_body,
      arg_name = "include"
    )

  test <-
    .formula_list_to_named_list(
      x = test,
      data = select(.extract_data_frame(x$inputs$data), any_of(include)),
      var_info = x$table_body,
      arg_name = "test",
      type_check = chuck(type_check, "is_function_or_string", "fn"),
      type_check_msg = chuck(type_check, "is_function_or_string", "msg")
    )

  estimate_fun <-
    .formula_list_to_named_list(
      x = {{ estimate_fun }},
      data = select(.extract_data_frame(x$inputs$data), any_of(x$meta_data$variable)),
      var_info = x$table_body,
      arg_name = "estimate_fun",
      type_check = chuck(type_check, "is_function", "fn"),
      type_check_msg = chuck(type_check, "is_function", "msg")
    )
  estimate_fun <-
    x$meta_data$variable %>%
    map(
      ~ estimate_fun[[.x]] %||%
        switch(x$meta_data[x$meta_data$variable %in% .x, ]$summary_type %in% "dichotomous" &&
          !identical(test[[.x]], "smd"),
        function(x) ifelse(!is.na(x), paste0(style_sigfig(x * 100), "%"), NA_character_)
        ) %||%
        style_sigfig
    ) %>%
    set_names(x$meta_data$variable)

  adj.vars <-
    .select_to_varnames(
      select = {{ adj.vars }},
      data = .extract_data_frame(x$inputs$data),
      var_info = x$table_body,
      arg_name = "adj.vars"
    )

  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("add_p(pvalue_fun=)")

  group <-
    .select_to_varnames(
      select = {{ group }},
      data = .extract_data_frame(x$inputs$data),
      var_info = x$table_body,
      arg_name = "group",
      select_single = TRUE
    )

  # checking for `tbl_summary(percent = c("cell", "row"))`, which don't apply
  if (!identical(x$inputs$percent, "column")) {
    bad_percent_vars <-
      filter(
        x$meta_data,
        .data$summary_type %in% c("categorical", "dichotomous"),
        .data$variable %in% include
      ) %>%
      pull("variable")
    if (!rlang::is_empty(bad_percent_vars)) {
      paste(
        "{.code add_difference()} results for categorical variables",
        "may not compatible with",
        "{.code tbl_summary(percent = c(\"cell\", \"row\"))} options.",
        "Use column percentages, {.code tbl_summary(percent = \"column\")}."
      ) %>%
        stringr::str_wrap() %>%
        cli_alert_info()
    }
  }

  # caller_env for add_p
  caller_env <- rlang::caller_env()

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    select("variable", "summary_type") %>%
    filter(.data$variable %in% include) %>%
    mutate(
      test = map2(
        .data$variable, .data$summary_type,
        function(variable, summary_type) {
          .assign_test_add_diff(
            data = x$inputs$data, variable = variable, summary_type = summary_type,
            by = x$by, group = group, test = test, adj.vars = adj.vars
          )
        }
      ),
      test_info = map(
        .data$test,
        function(test) {
          .get_add_p_test_fun(class(x)[1],
            test = test,
            env = caller_env,
            parent_fun = "add_difference"
          )
        }
      ),
      test_name = map_chr(.data$test_info, ~ pluck(.x, "test_name"))
    )
  # adding test_name to table body so it can be used to select vars by the test
  x$table_body <-
    left_join(x$table_body, meta_data[c("variable", "test_name")], by = "variable") %>%
    select("variable", "test_name", everything())

  # converting to named list
  test.args <-
    .formula_list_to_named_list(
      x = test.args,
      data = select(.extract_data_frame(x$inputs$data), any_of(include)),
      var_info = x$table_body,
      arg_name = "test.args",
      type_check = chuck(type_check, "is_named", "fn"),
      type_check_msg = chuck(type_check, "is_named", "msg")
    )

  x$meta_data <-
    meta_data %>%
    mutate(
      test_result = pmap(
        list(.data$test_info, .data$variable, .data$summary_type),
        function(test_info, variable, summary_type) {
          .run_add_p_test_fun(
            x = test_info, data = .env$x$inputs$data,
            by = .env$x$by, variable = variable,
            group = group, type = summary_type,
            test.args = test.args[[variable]],
            conf.level = conf.level, tbl = x,
            adj.vars = adj.vars
          )
        }
      )
    ) %>%
    select("variable", "test_result") %>%
    {
      left_join(x$meta_data, ., by = "variable")
    }

  x <-
    add_p_merge_p_values(
      x = x,
      lgl_add_p = FALSE,
      meta_data = x$meta_data,
      pvalue_fun = pvalue_fun,
      estimate_fun = estimate_fun,
      conf.level = conf.level,
      adj.vars = adj.vars
    )
  x$call_list <- updated_call_list

  # running any additional mods ------------------------------------------------
  x <-
    get_theme_element("add_difference-fn:addnl-fn-to-run", default = identity) %>%
    do.call(list(x))

  # return results -------------------------------------------------------------
  x
}
