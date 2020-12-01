

add_difference <- function(x, test = NULL, group = NULL,
                           conf.level = 0.95, adj.vars = NULL,
                           test.args = NULL, include = everything(),
                           pvalue_fun = NULL, estimate_fun = style_sigfig) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_summary"))
    stop("`x=` must be class 'tbl_summary'")
  if (is.null(x$by) || nrow(x$df_by) != 2)
    stop("'tbl_summary' object must have a `by=` value with exactly two levels")

  # expanding formula lists ----------------------------------------------------
  test <-
    .formula_list_to_named_list(
      x = test,
      data = select(x$inputs$data, any_of(include)),
      var_info = x$table_body,
      arg_name = "test"
    )

  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    getOption("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("add_p(pvalue_fun=)")

  estimate_fun <- gts_mapper(estimate_fun, "add_difference(estimate_fun=)")

  group <-
    .select_to_varnames(
      select = {{ group }},
      data = x$inputs$data,
      var_info = x$table_body,
      arg_name = "group",
      select_single = TRUE
    )
  include <-
    .select_to_varnames(
      select = {{ include }},
      data = select(x$inputs$data, any_of(x$meta_data$variable)),
      var_info = x$table_body,
      arg_name = "include"
    )
  # removing categorical variables
  if ("categorical" %in% dplyr::filter(x$meta_data, .data$variable %in% include)) {
    cat_vars <-
      dplyr::filter(x$meta_data,
                    .data$variable %in% include,
                    .data$summary_type %in% "categorical") %>%
      dplyr::pull(.data$variable)
    glue("Variable(s) {quoted_list(cat_vars)} are summary type 'categorical' ",
         "and not compatible with `add_difference()`.") %>%
      rlang::inform()
    include <- include %>% setdiff(cat_vars)
  }

  # caller_env for add_p
  caller_env <- rlang::caller_env()

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    select(.data$variable, .data$summary_type) %>%
    filter(.data$variable %in% include) %>%
    mutate(
      test = map2(
        .data$variable, .data$summary_type,
        function(variable, summary_type)
          .assign_test_add_diff(
            data = x$inputs$data, variable = variable, summary_type = summary_type,
            by = x$by, group = group, test = test)
      ),
      test_info = map(
        .data$test,
        function(test) .get_add_p_test_fun("tbl_summary", test = test, env = caller_env)
      ),
      test_name = map_chr(.data$test_info, ~pluck(.x, "test_name"))
    )
  # adding test_name to table body so it can be used to select vars by the test
  x$table_body <-
    left_join(x$table_body, meta_data[c("variable", "test_name")], by = "variable") %>%
    select(.data$variable, .data$test_name, everything())

  # converting to named list
  test.args <-
    .formula_list_to_named_list(
      x = test.args,
      data = select(x$inputs$data, any_of(include)),
      var_info = x$table_body,
      arg_name = "test.args"
    )

  x$meta_data <-
    meta_data %>%
    mutate(
      test_result = pmap(
        list(.data$test_info, .data$variable, .data$summary_type),
        function(test_info, variable, summary_type)
          .run_add_p_test_fun(x = test_info, data = .env$x$inputs$data,
                              by = .env$x$by, variable = variable,
                              group = group, type = summary_type,
                              test.args = test.args[[variable]],
                              conf.level = conf.level, tbl = x)
      )
    ) %>%
    select(.data$variable, .data$test_result) %>%
    {left_join(x$meta_data, ., by = "variable")}

  x$call_list <- c(x$call_list, list(add_p = match.call()))
  add_p_merge_p_values(x = x, meta_data = x$meta_data,
                       pvalue_fun = pvalue_fun,
                       estimate_fun = estimate_fun,
                       conf.level = conf.level)
}


