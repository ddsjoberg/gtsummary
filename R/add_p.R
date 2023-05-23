#' Adds p-values to gtsummary table
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @seealso [add_p.tbl_summary], [add_p.tbl_cross], [add_p.tbl_svysummary], [add_p.tbl_survfit], [add_p.tbl_continuous]
#' @export
add_p <- function(x, ...) {
  UseMethod("add_p")
}

#' Adds p-values to summary tables
#'
#' Adds p-values to tables created by `tbl_summary` by comparing values across groups.
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' @param test List of formulas specifying statistical tests to perform for each
#' variable,
#' e.g. \code{list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test")}.
#' Common tests include `"t.test"`, `"aov"`, `"wilcox.test"`, `"kruskal.test"`,
#' `"chisq.test"`, `"fisher.test"`, and `"lme4"` (for clustered data). See [tests]
#' for details, more tests, and instruction for implementing a custom test.
#'
#' Tests default to `"kruskal.test"` for continuous variables (`"wilcox.test"`
#' when "`by`" variable has two levels), `"chisq.test.no.correct"` for
#' categorical variables with all expected cell counts >=5, and `"fisher.test"`
#' for categorical variables with any expected cell count <5.
#' @param group Column name (unquoted or quoted) of an ID or grouping variable.
#' The column can be used to calculate p-values with correlated data.
#' Default is `NULL`. See [tests] for methods that utilize the `group=` argument.
#' @param test.args List of formulas containing additional arguments to pass to
#' tests that accept arguments. For example, add an argument for all t-tests,
#' use `test.args = all_tests("t.test") ~ list(var.equal = TRUE)`
#' @param ... Not used
#' @inheritParams tbl_regression
#' @inheritParams tbl_summary
#' @family tbl_summary tools
#' @seealso See tbl_summary \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for detailed examples
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @export
#' @return A `tbl_summary` object
#' @author Daniel D. Sjoberg, Emily C. Zabor
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' add_p_ex1 <-
#'   trial[c("age", "grade", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' add_p_ex2 <-
#'   trial %>%
#'   select(trt, age, marker) %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_p(
#'     # perform t-test for all variables
#'     test = everything() ~ "t.test",
#'     # assume equal variance in the t-test
#'     test.args = all_tests("t.test") ~ list(var.equal = TRUE)
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_ex1.png", width = "60")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_ex2.png", width = "60")`
#' }}

add_p.tbl_summary <- function(x, test = NULL, pvalue_fun = NULL,
                              group = NULL, include = everything(),
                              test.args = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # setting defaults from gtsummary theme --------------------------------------
  test <- test %||% get_theme_element("add_p.tbl_summary-arg:test")
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("add_p(pvalue_fun=)")

  # converting bare arguments to string ----------------------------------------
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

  # checking that input x has a by var
  if (is.null(x$df_by)) {
    paste(
      "Cannot add a p-value when no 'by' variable",
      "in original `tbl_summary(by=)` call."
    ) %>%
      stop(call. = FALSE)
  }
  if (any(c("add_difference", "add_p") %in% names(x$call_list)) &&
    "p.value" %in% names(x$table_body)) {
    paste(
      "`add_p()` cannot be run after `add_difference()` or `add_p()` when a",
      "'p.value' column is already present."
    ) %>%
      stop(call. = FALSE)
  }

  # test -----------------------------------------------------------------------
  # parsing into a named list
  test <-
    .formula_list_to_named_list(
      x = test,
      data = select(x$inputs$data, any_of(include)),
      var_info = x$table_body,
      arg_name = "test",
      type_check = chuck(type_check, "is_function_or_string", "fn"),
      type_check_msg = chuck(type_check, "is_function_or_string", "msg")
    )

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.", call. = FALSE)
  }

  # caller_env for add_p
  caller_env <- rlang::caller_env()

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    select("variable", "summary_type") %>%
    filter(.data$variable %in% include) %>%
    mutate(
      test =
        map2(
          .data$variable, .data$summary_type,
          function(variable, summary_type) {
            .assign_test_tbl_summary(
              data = x$inputs$data, variable = variable, summary_type = summary_type,
              by = x$by, group = group, test = test
            )
          }
        ),
      test_info =
        map(
          .data$test,
          function(test) .get_add_p_test_fun("tbl_summary", test = test, env = caller_env)
        ),
      test_name = map_chr(.data$test_info, ~ pluck(.x, "test_name"))
    )
  # adding test_name to table body so it can be used to select vars by the test
  x$table_body <-
    x$table_body %>%
    select(-any_of(c("test_name", "test_result"))) %>%
    left_join(meta_data[c("variable", "test_name")], by = "variable") %>%
    select("variable", "test_name", everything())

  # converting to named list
  test.args <-
    .formula_list_to_named_list(
      x = test.args,
      data = select(x$inputs$data, any_of(include)),
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
            test.args = test.args[[variable]], tbl = x
          )
        }
      ),
      p.value = map_dbl(.data$test_result, ~ pluck(.x, "df_result", "p.value")),
      stat_test_lbl = map_chr(.data$test_result, ~ pluck(.x, "df_result", "method"))
    ) %>%
    select("variable", "test_result", "p.value", "stat_test_lbl") %>%
    {
      left_join(
        x$meta_data %>% select(-any_of(c("test_result", "p.value", "stat_test_lbl"))),
        .,
        by = "variable"
      )
    }

  x$call_list <- updated_call_list
  add_p_merge_p_values(x, meta_data = x$meta_data, pvalue_fun = pvalue_fun)
}

# function to create text for footnote
footnote_add_p <- function(meta_data) {
  if (!"test_result" %in% names(meta_data)) {
    return(NA_character_)
  }

  footnotes <-
    meta_data$test_result %>%
    map_chr(~ pluck(., "df_result", "method") %||% NA_character_) %>%
    stats::na.omit() %>%
    unique()

  if (length(footnotes) > 0) {
    language <- get_theme_element("pkgwide-str:language", default = "en")
    return(paste(map_chr(footnotes, ~ translate_text(.x, language)), collapse = "; "))
  } else {
    return(NA_character_)
  }
}

# function to merge p-values to tbl
add_p_merge_p_values <- function(x, lgl_add_p = TRUE,
                                 meta_data, pvalue_fun,
                                 estimate_fun = NULL,
                                 conf.level = 0.95,
                                 adj.vars = NULL) {
  x <-
    # merging in p-value to table_body
    modify_table_body(
      x,
      left_join,
      meta_data %>%
        select("variable", "test_result") %>%
        mutate(
          df_result = map(.data$test_result, ~ pluck(.x, "df_result")),
          row_type = "label"
        ) %>%
        unnest("df_result") %>%
        select(
          -any_of("method"),
          # removing any columns already present in table_body
          -any_of(names(x$table_body) %>% setdiff(c("variable", "row_type")))
        ),
      by = c("variable", "row_type")
    ) %>%
    # adding print instructions for p-value column
    modify_table_styling(
      columns = any_of("p.value"),
      label = paste0("**", translate_text("p-value"), "**"),
      hide = FALSE,
      fmt_fun = pvalue_fun,
      footnote = footnote_add_p(meta_data)
    )

  # don't display difference and CI for add_p fns
  if (lgl_add_p == FALSE) {
    x <- x %>%
      # adding print instructions for estimate
      modify_table_styling(
        columns = any_of("estimate"),
        label = ifelse(is.null(adj.vars),
          paste0("**", translate_text("Difference"), "**"),
          paste0("**", translate_text("Adjusted Difference"), "**")
        ),
        hide = FALSE,
        fmt_fun = switch(is_function(estimate_fun),
          estimate_fun
        ),
        footnote = footnote_add_p(meta_data)
      )

    # add row formatting for difference and CI
    if (is.list(estimate_fun)) {
      x$table_styling$fmt_fun <-
        x$table_styling$fmt_fun %>%
        bind_rows(
          estimate_fun %>%
            tibble::enframe("variable", "fmt_fun") %>%
            rowwise() %>%
            mutate(
              column =
                c("estimate", "conf.low", "conf.high") %>%
                  intersect(names(x$table_body)) %>%
                  list(),
              rows = glue(".data$variable == '{variable}'") %>% rlang::parse_expr() %>% list()
            ) %>%
            ungroup() %>%
            select("column", "rows", "fmt_fun") %>%
            unnest(cols = "column")
        )
    }


    # adding formatted CI column
    if (all(c("conf.low", "conf.high") %in% names(x$table_body)) &&
      !"ci" %in% names(x$table_body)) {
      ci.sep <- get_theme_element("pkgwide-str:ci.sep", default = ", ")
      x <- x %>%
        modify_table_body(
          ~ .x %>%
            mutate(
              ci = pmap_chr(
                list(variable, conf.low, conf.high),
                ~ case_when(
                  !is.na(..2) | !is.na(..3) ~
                    paste(do.call(estimate_fun[[..1]], list(..2)),
                      do.call(estimate_fun[[..1]], list(..3)),
                      sep = ci.sep
                    )
                )
              )
            )
        ) %>%
        modify_table_body(dplyr::relocate, "ci", .before = "conf.low") %>%
        # adding print instructions for estimates
        modify_table_styling(
          any_of("ci"),
          label = paste0("**", conf.level * 100, "% ", translate_text("CI"), "**"),
          hide = FALSE,
          footnote = footnote_add_p(meta_data),
          footnote_abbrev = translate_text("CI = Confidence Interval")
        )
    }
  }

  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x
}


#' Adds p-value to crosstab table
#'
#' Calculate and add a p-value comparing the two variables in the cross table.
#' Missing values are included in p-value calculations.
#'
#' @param x Object with class `tbl_cross` from the [tbl_cross] function
#' @param pvalue_fun Function to round and format p-value.
#' Default is [style_pvalue], except when `source_note = TRUE` when the
#' default is `style_pvalue(x, prepend_p = TRUE)`
#' @param source_note Logical value indicating whether to show p-value
#' in the \{gt\} table source notes rather than a column.
#' @param test A string specifying statistical test to perform. Default is
#' "`chisq.test`" when expected cell counts >=5 and "`fisher.test`" when
#' expected cell counts <5.
#' @param test.args Named list containing additional arguments to pass to
#' the test (if it accepts additional arguments).
#' For example, add an argument for a chi-squared test with
#' `test.args = list(correct = TRUE)`
#' @inheritParams add_p.tbl_summary
#' @family tbl_cross tools
#' @author Karissa Whiting
#' @export
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' add_p_cross_ex1 <-
#'   trial %>%
#'   tbl_cross(row = stage, col = trt) %>%
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' add_p_cross_ex2 <-
#'   trial %>%
#'   tbl_cross(row = stage, col = trt) %>%
#'   add_p(source_note = TRUE)
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_cross_ex1.png", width = "50")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_cross_ex2.png", width = "45")`
#' }}
add_p.tbl_cross <- function(x, test = NULL, pvalue_fun = NULL,
                            source_note = NULL,
                            test.args = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # setting defaults -----------------------------------------------------------
  test <- test %||% get_theme_element("add_p.tbl_cross-arg:test")
  source_note <- source_note %||%
    get_theme_element("add_p.tbl_cross-arg:source_note", default = FALSE)
  if (source_note == FALSE) {
    pvalue_fun <-
      pvalue_fun %||%
      get_theme_element("add_p.tbl_cross-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun") %||%
      .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue) %>%
      gts_mapper("add_p(pvalue_fun=)")
  } else {
    pvalue_fun <-
      pvalue_fun %||%
      get_theme_element("pkgwide-fn:prependpvalue_fun") %||%
      (function(x) style_pvalue(x, prepend_p = TRUE)) %>%
      gts_mapper("add_p(pvalue_fun=)")
  }

  # adding test name if supplied (NULL otherwise)
  input_test <- switch(!is.null(test),
    rlang::expr(everything() ~ !!test)
  )
  input_test.args <- switch(!is.null(test.args),
    rlang::expr(everything() ~ !!test.args)
  )

  # running add_p to add the p-value to the output
  x_copy <- x
  # passing the data frame after missing values have been transformed to factor/observed levels
  x$inputs$data <- x$tbl_data
  x <-
    expr(
      add_p.tbl_summary(x,
        test = !!input_test,
        test.args = !!input_test.args,
        pvalue_fun = !!pvalue_fun,
        include = -any_of("..total..")
      )
    ) %>%
    eval()
  # replacing the input data set with the original from the `tbl_cross()` call
  x$inputs$data <- x_copy$inputs$data

  if (source_note == TRUE) {
    test_name <-
      x$meta_data$stat_test_lbl %>%
      discard(is.na) %>%
      translate_text()
    #  report p-value as a source_note
    # hiding p-value from output
    x <-
      modify_table_styling(
        x,
        columns = "p.value",
        footnote = NA_character_,
        hide = TRUE
      )

    x$table_styling$source_note <-
      paste(test_name, pvalue_fun(discard(x$meta_data$p.value, is.na)), sep = ", ")
    attr(x$table_styling$source_note, "text_interpret") <- "md"
  }

  # strip markdown bold around column label ------------------------------------
  x$table_styling$header <-
    x$table_styling$header %>%
    mutate(
      label =
        ifelse(
          .data$column %in% "p.value",
          stringr::str_replace_all(.data$label,
            pattern = "\\*\\*(.*?)\\*\\*",
            replacement = "\\1"
          ),
          .data$label
        )
    )

  # return tbl_cross -----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}


#' Adds p-value to survfit table
#'
#' \lifecycle{maturing}
#' Calculate and add a p-value
#' @param x Object of class `"tbl_survfit"`
#' @param test string indicating test to use. Must be one of `"logrank"`, `"tarone"`, `"survdiff"`,
#' `"petopeto_gehanwilcoxon"`, `"coxph_lrt"`, `"coxph_wald"`, `"coxph_score".`
#' See details below
#' @inheritParams add_p.tbl_summary
#' @inheritParams combine_terms
#' @family tbl_survfit tools
#'
#' @section test argument:
#' The most common way to specify `test=` is by using a single string indicating
#' the test name. However, if you need to specify different tests within the same
#' table, the input in flexible using the list notation common throughout the
#' gtsummary package. For example, the following code would call the log-rank test,
#' and a second test of the *G-rho* family.
#' ```r
#' ... %>%
#'   add_p(test = list(trt ~ "logrank", grade ~ "survdiff"),
#'         test.args = grade ~ list(rho = 0.5))
#' ```
#'
#' @export
#' @examplesIf broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE)
#' \donttest{
#' library(survival)
#'
#' gts_survfit <-
#'   list(
#'     survfit(Surv(ttdeath, death) ~ grade, trial),
#'     survfit(Surv(ttdeath, death) ~ trt, trial)
#'   ) %>%
#'   tbl_survfit(times = c(12, 24))
#'
#' # Example 1 ----------------------------------
#' add_p_tbl_survfit_ex1 <-
#'   gts_survfit %>%
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' # Pass `rho=` argument to `survdiff()`
#' add_p_tbl_survfit_ex2 <-
#'   gts_survfit %>%
#'   add_p(test = "survdiff", test.args = list(rho = 0.5))
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_tbl_survfit_ex1.png", width = "55")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_tbl_survfit_ex2.png", width = "45")`
#' }}

add_p.tbl_survfit <- function(x, test = "logrank", test.args = NULL,
                              pvalue_fun = style_pvalue,
                              include = everything(), quiet = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  updated_call_list <- c(x$call_list, list(add_p = match.call()))
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking inputs ------------------------------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("add_p(pvalue_fun=)")

  include <- .select_to_varnames(select = {{ include }}, var_info = x$meta_data)

  # if user passed a string of the test name, convert it to a tidy select list
  if (rlang::is_string(test)) {
    test <- expr(everything() ~ !!test) %>% eval()
    if (!is.null(test.args)) {
      test.args <- expr(everything() ~ !!test.args) %>% eval()
    }
  }

  # converting test and test.args to named list --------------------------------
  test <-
    .formula_list_to_named_list(
      x = test,
      var_info = x$table_body,
      arg_name = "test",
      type_check = chuck(type_check, "is_function_or_string", "fn"),
      type_check_msg = chuck(type_check, "is_function_or_string", "msg")
    )

  # adding pvalue to meta data -------------------------------------------------
  # caller_env for add_p
  caller_env <- rlang::caller_env()

  # getting the test name and p-value
  meta_data <-
    x$meta_data %>%
    filter(.data$stratified == TRUE & .data$variable %in% include) %>%
    select("variable", "survfit") %>%
    mutate(
      test = map(.data$variable, ~ test[[.x]] %||% "logrank"),
      test_info = map(
        .data$test,
        function(test) .get_add_p_test_fun("tbl_survfit", test = test, env = caller_env)
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
      var_info = x$table_body,
      arg_name = "test.args",
      type_check = chuck(type_check, "is_named", "fn"),
      type_check_msg = chuck(type_check, "is_named", "msg")
    )

  # checking the formula and data from survfit object are available
  purrr::walk(
    x$meta_data$survfit,
    function(suvfit) {
      # extracting survfit call
      survfit_call <- suvfit$call %>% as.list()
      # index of formula and data
      call_index <- names(survfit_call) %in% c("formula", "data") %>% which()
      # converting call into a model.frame call
      rlang::call2(rlang::expr(stats::model.frame), !!!survfit_call[call_index]) %>%
        safe_survfit_eval()
    }
  )

  x$meta_data <-
    meta_data %>%
    mutate(
      test_result = pmap(
        list(.data$test_info, .data$variable, .data$survfit),
        function(test_info, variable, survfit) {
          .run_add_p_test_fun(
            x = test_info, data = survfit,
            variable = variable,
            test.args = test.args[[variable]]
          )
        }
      ),
      p.value = map_dbl(.data$test_result, ~ pluck(.x, "df_result", "p.value")),
      stat_test_lbl = map_chr(.data$test_result, ~ pluck(.x, "df_result", "method"))
    ) %>%
    select("variable", "test_result", "p.value", "stat_test_lbl") %>%
    {
      left_join(x$meta_data, ., by = "variable")
    }

  x$call_list <- updated_call_list
  add_p_merge_p_values(x, meta_data = x$meta_data, pvalue_fun = pvalue_fun)
}


#' Adds p-values to svysummary tables
#'
#' Adds p-values to tables created by `tbl_svysummary` by comparing values across groups.
#'
#' @param x Object with class `tbl_svysummary` from the [tbl_svysummary] function
#' @param test List of formulas specifying statistical tests to perform,
#' e.g. \code{list(all_continuous() ~ "svy.t.test", all_categorical() ~ "svy.wald.test")}.
#' Options include
#' * `"svy.t.test"` for a t-test adapted to complex survey samples (cf. `survey::svyttest`),
#' * `"svy.wilcox.test"` for a Wilcoxon rank-sum test for complex survey samples (cf. `survey::svyranktest`),
#' * `"svy.kruskal.test"` for a Kruskal-Wallis rank-sum test for complex survey samples (cf. `survey::svyranktest`),
#' * `"svy.vanderwaerden.test"` for a van der Waerden's normal-scores test for complex survey samples (cf. `survey::svyranktest`),
#' * `"svy.median.test"` for a Mood's test for the median for complex survey samples (cf. `survey::svyranktest`),
#' * `"svy.chisq.test"` for a Chi-squared test with Rao & Scott's second-order correction (cf. `survey::svychisq`),
#' * `"svy.adj.chisq.test"` for a Chi-squared test adjusted by a design effect estimate (cf. `survey::svychisq`),
#' * `"svy.wald.test"` for a Wald test of independence for complex survey samples (cf. `survey::svychisq`),
#' * `"svy.adj.wald.test"` for an adjusted Wald test of independence for complex survey samples (cf. `survey::svychisq`),
#' * `"svy.lincom.test"` for a test of independence using the exact asymptotic distribution for complex survey samples (cf. `survey::svychisq`),
#' * `"svy.saddlepoint.test"` for a test of independence using a saddlepoint approximation for complex survey samples (cf. `survey::svychisq`),
#'
#' Tests default to `"svy.wilcox.test"` for continuous variables and `"svy.chisq.test"`
#' for categorical variables.
#' @param ... Not used
#' @inheritParams add_p.tbl_summary
#' @inheritParams tbl_regression
#' @inheritParams tbl_svysummary
#' @family tbl_svysummary tools
#' @export
#' @return A `tbl_svysummary` object
#' @author Joseph Larmarange
#' @examplesIf broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE)
#' \donttest{
#' # Example 1 ----------------------------------
#' # A simple weighted dataset
#' add_p_svysummary_ex1 <-
#'   survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) %>%
#'   tbl_svysummary(by = Survived, include = c(Sex, Age)) %>%
#'   add_p()
#'
#' # A dataset with a complex design
#' data(api, package = "survey")
#' d_clust <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#'
#' # Example 2 ----------------------------------
#' add_p_svysummary_ex2 <-
#'   tbl_svysummary(d_clust, by = both, include = c(api00, api99, both)) %>%
#'   add_p()
#'
#' # Example 3 ----------------------------------
#' # change tests to svy t-test and Wald test
#' add_p_svysummary_ex3 <-
#'   tbl_svysummary(d_clust, by = both, include = c(cname, api00, api99, both)) %>%
#'   add_p(
#'     test = list(
#'       all_continuous() ~ "svy.t.test",
#'       all_categorical() ~ "svy.wald.test"
#'     )
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_svysummary_ex1.png", width = "45")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_svysummary_ex2.png", width = "65")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_p_svysummary_ex3.png", width = "60")`
#' }}

add_p.tbl_svysummary <- function(x, test = NULL, pvalue_fun = NULL,
                                 include = everything(), test.args = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  updated_call_list <- c(x$call_list, list(add_p = match.call()))

  # checking for survey package ------------------------------------------------
  assert_package("survey", "add_p.tbl_svysummary()")

  # setting defaults from gtsummary theme --------------------------------------
  test <- test %||%
    get_theme_element("add_p.tbl_svysummary-arg:test") %||%
    get_theme_element("add_p.tbl_summary-arg:test")
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("add_p.tbl_svysummary-arg:pvalue_fun") %||%
    get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("add_p(pvalue_fun=)")


  # converting bare arguments to string ----------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      data = select(x$inputs$data$variables, any_of(x$meta_data$variable)),
      var_info = x$table_body,
      arg_name = "include"
    )

  # checking that input x has a by var
  if (is.null(x$df_by)) {
    stop(paste0(
      "Cannot add comparison when no 'by' variable ",
      "in original `tbl_svysummary()` call"
    ), call. = FALSE)
  }

  # test -----------------------------------------------------------------------
  # parsing into a named list
  test <-
    .formula_list_to_named_list(
      x = test,
      data = select(x$inputs$data$variables, any_of(x$meta_data$variable)),
      var_info = x$table_body,
      arg_name = "test",
      type_check = chuck(type_check, "is_function_or_string", "fn"),
      type_check_msg = chuck(type_check, "is_function_or_string", "msg")
    )

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.", call. = FALSE)
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
          .assign_test_tbl_svysummary(
            data = x$inputs$data, variable = variable, summary_type = summary_type,
            by = x$by, test = test
          )
        }
      ),
      test_info = map(
        .data$test,
        function(test) .get_add_p_test_fun("tbl_svysummary", test = test, env = caller_env)
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
      data = select(x$inputs$data, any_of(include)),
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
            by = .env$x$inputs$by, variable = variable,
            type = summary_type,
            test.args = test.args[[variable]]
          )
        }
      ),
      p.value = map_dbl(.data$test_result, ~ pluck(.x, "df_result", "p.value")),
      stat_test_lbl = map_chr(.data$test_result, ~ pluck(.x, "df_result", "method"))
    ) %>%
    select("variable", "test_result", "p.value", "stat_test_lbl") %>%
    {
      left_join(x$meta_data, ., by = "variable")
    }

  x$call_list <- updated_call_list
  add_p_merge_p_values(x, meta_data = x$meta_data, pvalue_fun = pvalue_fun)
}

#' P-values for `tbl_continuous`
#'
#' @inheritParams add_p.tbl_summary
#' @param test List of formulas specifying statistical tests to perform for each
#' variable.
#' Default is two-way ANOVA when `by=` is not `NULL`, and has the same defaults
#' as `add_p.tbl_continuous()` when `by = NULL`.
#' See [tests] for details, more tests, and instruction for implementing a custom test.
#' @export
#' @family tbl_continuous tools
#' @examples
#' add_p_continuous_ex1 <-
#'   tbl_continuous(
#'     data = trial,
#'     variable = age,
#'     by = trt,
#'     include = grade
#'   ) %>%
#'   add_p()
add_p.tbl_continuous <- function(x, test = NULL, pvalue_fun = NULL,
                                 include = everything(), test.args = NULL,
                                 group = NULL, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  updated_call_list <- c(x$call_list, list(add_p = match.call()))
  # setting defaults from gtsummary theme --------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("pkgwide-fn:pvalue_fun", default = style_pvalue) %>%
    gts_mapper("add_p(pvalue_fun=)")

  # converting bare arguments to string ----------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      data = select(x$inputs$data, any_of(x$meta_data$variable)),
      var_info = x$table_body,
      arg_name = "include"
    )

  group <-
    .select_to_varnames(
      select = {{ group }},
      data = x$inputs$data,
      arg_name = "group"
    )

  test <-
    .formula_list_to_named_list(
      x = test,
      data = select(x$inputs$data, any_of(x$meta_data$variable)),
      var_info = x$table_body,
      arg_name = "test",
      type_check = chuck(type_check, "is_function_or_string", "fn"),
      type_check_msg = chuck(type_check, "is_function_or_string", "msg")
    )

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.", call. = FALSE)
  }

  # caller_env for add_p
  caller_env <- rlang::caller_env()

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    select("variable", "summary_type") %>%
    filter(.data$variable %in% .env$include) %>%
    mutate(
      test =
        map(
          .data$variable,
          function(variable) {
            .assign_test_tbl_continuous(
              data = x$inputs$data, continuous_variable = x$inputs$variable,
              variable = variable,
              by = x$inputs$by, group = group, test = test
            )
          }
        ),
      test_info =
        map(
          .data$test,
          function(test) .get_add_p_test_fun("tbl_continuous", test = test, env = caller_env)
        ),
      test_name = map_chr(.data$test_info, ~ pluck(.x, "test_name"))
    )

  # adding test_name to table body so it can be used to select vars by the test
  x$table_body <-
    x$table_body %>%
    select(-any_of(c("test_name", "test_result"))) %>%
    left_join(meta_data[c("variable", "test_name")], by = "variable") %>%
    select("variable", "test_name", everything())

  # converting to named list
  test.args <-
    .formula_list_to_named_list(
      x = test.args,
      data = select(x$inputs$data, any_of(include)),
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
            by = .env$x$inputs$by, variable = variable,
            group = group, type = summary_type,
            test.args = test.args[[variable]], tbl = x,
            continuous_variable = x$inputs$variable
          )
        }
      ),
      p.value = map_dbl(.data$test_result, ~ pluck(.x, "df_result", "p.value")),
      stat_test_lbl = map_chr(.data$test_result, ~ pluck(.x, "df_result", "method"))
    ) %>%
    select("variable", "test_result", "p.value", "stat_test_lbl") %>%
    {
      left_join(
        x$meta_data %>% select(-any_of(c("test_result", "p.value", "stat_test_lbl"))),
        .,
        by = "variable"
      )
    }

  x$call_list <- updated_call_list
  add_p_merge_p_values(x, meta_data = x$meta_data, pvalue_fun = pvalue_fun)
}
