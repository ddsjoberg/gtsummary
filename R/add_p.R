#' Adds p-values to gtsummary table
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @author Daniel D. Sjoberg
#' @seealso [add_p.tbl_summary], [add_p.tbl_cross], [add_p.tbl_svysummary]
#' @export
add_p <- function(x, ...) {
  UseMethod("add_p")
}

#' Adds p-values to summary tables
#'
#' Adds p-values to tables created by `tbl_summary` by comparing values across groups.
#'
#' @section Setting Defaults:
#' If you like to consistently use a different function to format p-values or
#' estimates, you can set options in the script or in the user- or
#' project-level start-up file, '.Rprofile'.  The default confidence level can
#' also be set. Please note the default option for the estimate is the same
#' as it is for `tbl_regression()`.
#' \itemize{
#'   \item `options(gtsummary.pvalue_fun = new_function)`
#' }
#'
#' @param x Object with class `tbl_summary` from the [tbl_summary] function
#' @param test List of formulas specifying statistical tests to perform,
#' e.g. \code{list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test")}.
#' Options include
#' * `"t.test"` for a t-test,
#' * `"aov"` for a one-way ANOVA test,
#' * `"wilcox.test"` for a Wilcoxon rank-sum test,
#' * `"kruskal.test"` for a Kruskal-Wallis rank-sum test,
#' * `"chisq.test"` for a chi-squared test of independence,
#' * `"chisq.test.no.correct"` for a chi-squared test of independence without continuity correction,
#' * `"fisher.test"` for a Fisher's exact test,
#' * `"lme4"` for a random intercept logistic regression model to account for
#' clustered data, `lme4::glmer(by ~ variable + (1 | group), family = binomial)`.
#' The `by` argument must be binary for this option.
#'
#' Tests default to `"kruskal.test"` for continuous variables, `"chisq.test"` for
#' categorical variables with all expected cell counts >=5, and `"fisher.test"`
#' for categorical variables with any expected cell count <5.
#' A custom test function can be added for all or some variables. See below for
#' an example.
#' @param group Column name (unquoted or quoted) of an ID or grouping variable.
#' The column can be used to calculate p-values with correlated data (e.g. when
#' the test argument is `"lme4"`). Default is `NULL`.  If specified,
#' the row associated with this variable is omitted from the summary table.
#' @param ... Not used
#' @inheritParams tbl_regression
#' @inheritParams tbl_summary
#' @family tbl_summary tools
#' @seealso See tbl_summary \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for detailed examples
#' @export
#' @return A `tbl_summary` object
#' @author Emily C. Zabor, Daniel D. Sjoberg
#' @examples
#' # Example 1 ----------------------------------
#' add_p_ex1 <-
#'   trial[c("age", "grade", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p()
#'
#' # Example 2 ----------------------------------
#' # Conduct a custom McNemar test for response,
#' # Function must return a named list of the p-value and the
#' # test name: list(p = 0.123, test = "McNemar's test")
#' # The '...' must be included as input
#' # This feature is experimental, and the API may change in the future
#' my_mcnemar <- function(data, variable, by, ...) {
#'   result <- list()
#'   result$p <- stats::mcnemar.test(data[[variable]], data[[by]])$p.value
#'   result$test <- "McNemar's test"
#'   result
#' }
#'
#' add_p_ex2 <-
#'   trial[c("response", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p(test = response ~ "my_mcnemar")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_p_ex1.png}{options: width=60\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_p_ex2.png}{options: width=60\%}}

add_p.tbl_summary <- function(x, test = NULL, pvalue_fun = NULL,
                  group = NULL, include = everything(), exclude = NULL, ...) {

  # DEPRECATION notes ----------------------------------------------------------
  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::add_p(exclude = )",
      "add_p(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude variable, use the minus sign.\n",
        "For example, `include = -c(age, stage)`"
      )
    )
  }

  # setting defaults from gtsummary theme --------------------------------------
  test <- test %||% get_theme_element("add_p.tbl_summary-arg:test")
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun")


  # converting bare arguments to string ----------------------------------------
  group <- var_input_to_string(data = x$inputs$data,
                               select_input = !!rlang::enquo(group),
                               arg_name = "group", select_single = TRUE)
  include <- var_input_to_string(data = select(x$inputs$data, any_of(x$meta_data$variable)),
                                 select_input = !!rlang::enquo(include),
                                 arg_name = "include")
  exclude <- var_input_to_string(data = select(x$inputs$data, any_of(x$meta_data$variable)),
                                 select_input = !!rlang::enquo(exclude),
                                 arg_name = "exclude")

  # group argument -------------------------------------------------------------
  if (!is.null(group)) {
    # checking group is in the data frame
    if (!group %in% names(x$inputs$data)) {
      stop(glue("'{group}' is not a column name in the input data frame."), call. = FALSE)
    }
    if (group %in% x$meta_data$variable) {
      rlang::inform(glue::glue(
        "The `group=` variable is no longer auto-removed from the summary table as of v1.3.1.\n",
        "The following syntax is now preferred:\n",
        "tbl_summary(..., include = -{group}) %>% add_p(..., group = {group})"))
    }
  }

  # setting defaults -----------------------------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    getOption("gtsummary.pvalue_fun", default = style_pvalue)
  if (!rlang::is_function(pvalue_fun)) {
    stop(paste0(
      "'pvalue_fun' is not a valid function.  Please pass only a function\n",
      "object. For example,\n\n",
      "'pvalue_fun = function(x) style_pvalue(x, digits = 2)'"
    ), call. = FALSE)
  }

  # checking that input x has a by var
  if (is.null(x$df_by)) {
    stop(paste0(
      "Cannot add comparison when no 'by' variable ",
      "in original tbl_summary() call"
    ), call. = FALSE)
  }

  # test -----------------------------------------------------------------------
  # parsing into a named list
  test <- tidyselect_to_list(
    select(x$inputs$data, any_of(x$meta_data$variable)),
    test, .meta_data = x$meta_data, arg_name = "test"
  )

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.", call. = FALSE)
  }

  # Getting p-values only for included variables
  include <- include %>% setdiff(exclude)

  # caller_env for add_p
  caller_env <- rlang::caller_env()

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    mutate(
      # assigning statistical test to perform
      stat_test = assign_test(
        data = x$inputs$data,
        var = .data$variable,
        var_summary_type = .data$summary_type,
        by_var = x$by,
        test = test,
        group = group,
        env = caller_env
      ),
      # calculating pvalue
      test_result = calculate_pvalue(
        data = x$inputs$data,
        variable = .data$variable,
        by = x$by,
        test = .data$stat_test,
        type = .data$summary_type,
        group = group,
        include = include
      ),
      # grabbing p-value and test label from test_result
      p.value = map_dbl(
        .data$test_result,
        ~ pluck(.x, "p") %||% switch(is.numeric(.x), .x[1]) %||% NA_real_
      ),
      stat_test_lbl = map_chr(
        .data$test_result,
        ~ pluck(.x, "test") %||% NA_character_
      )
    ) %>%
    select(-.data$test_result)

  add_p_merge_p_values(x, meta_data, pvalue_fun)
}

# function to create text for footnote
footnote_add_p <- function(meta_data) {
  meta_data$stat_test_lbl %>%
    keep(~ !is.na(.)) %>%
    unique() %>%
    paste(collapse = "; ") %>%
    paste0(translate_text("Statistical tests performed"), ": ", .)
}

# function to merge p-values to tbl
add_p_merge_p_values <- function(x, meta_data, pvalue_fun){
  # creating pvalue column for table_body merge
  pvalue_column <-
    meta_data %>%
    select(c("variable", "p.value")) %>%
    mutate(row_type = "label")

  table_body <-
    x$table_body %>%
    left_join(
      pvalue_column,
      by = c("variable", "row_type")
    )

  x$table_body <- table_body
  x$meta_data <- meta_data

  x$table_header <-
    tibble(column = names(table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing() %>%
    table_header_fmt_fun(p.value = pvalue_fun) %>%
    mutate(
      footnote = ifelse(.data$column == "p.value",
                        footnote_add_p(meta_data),
                        .data$footnote)
    )

  # updating header
  x <- modify_header_internal(x, p.value = paste0("**", translate_text("p-value"), "**"))

  x$call_list <- c(x$call_list, list(add_p = match.call()))

  x
}


#' Adds p-value to crosstab table
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
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
#' @inheritParams add_p.tbl_summary
#' @family tbl_cross tools
#' @author Karissa Whiting
#' @export
#' @examples
#' add_p_cross_ex1 <-
#'   trial %>%
#'   tbl_cross(row = stage, col = trt) %>%
#'   add_p()
#'
#' add_p_cross_ex2 <-
#'   trial %>%
#'   tbl_cross(row = stage, col = trt) %>%
#'   add_p(source_note = TRUE)
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_p_cross_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_p_cross_ex2.png}{options: width=45\%}}
add_p.tbl_cross <- function(x, test = NULL, pvalue_fun = NULL,
                            source_note = FALSE, ...) {
  # setting defaults -----------------------------------------------------------
  test <- test %||% get_theme_element("add_p.tbl_cross-arg:test")
  if (source_note == FALSE)
    pvalue_fun <-
      pvalue_fun %||%
      getOption("gtsummary.pvalue_fun", default = style_pvalue)  %||%
      get_theme_element("add_p.tbl_cross-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun")
  else
    pvalue_fun <-
      pvalue_fun %||%
      get_theme_element("pkgwide-fn:prependpvalue_fun") %||%
      (function(x) style_pvalue(x, prepend_p = TRUE))

  # adding test name if supplied (NULL otherwise)
  input_test <- switch(!is.null(test),
                       rlang::expr(everything() ~ !!test))

  # running add_p to add the p-value to the output
  x_copy <- x
  # passing the data frame after missing values have been transformed to factor/observed levels
  x$inputs$data <- x$tbl_data
  x <- expr(add_p.tbl_summary(x, test = !!input_test, include = -any_of("..total.."))) %>% eval()
  # replacing the input dataset with the original from the `tbl_cross()` call
  x$inputs$data <- x_copy$inputs$data

  # updating footnote
  test_name <- x$meta_data$stat_test_lbl %>% discard(is.na)
  x$table_header <-
    x$table_header %>%
    mutate(
      footnote = ifelse(.data$column == "p.value",
                        test_name, .data$footnote)
    )


  if (source_note == TRUE) {
    #  report p-value as a source_note
    # hiding p-value from output
    x$table_header <-
      x$table_header %>%
      mutate(
        hide = ifelse(.data$column == "p.value", TRUE, .data$hide),
        footnote = ifelse(.data$column == "p.value", NA_character_, .data$footnote),
      )

    x$list_output$source_note <-
      paste(test_name, pvalue_fun(discard(x$meta_data$p.value, is.na)), sep = ", ")
  }

  # return tbl_cross
  x[["call_list"]] <- list(x[["call_list"]], add_p = match.call())
  x
}


#' Adds p-values to svysummary tables
#'
#' Adds p-values to tables created by `tbl_svysummary` by comparing values across groups.
#'
#'
#' @param x Object with class `tbl_svysummary` from the [tbl_svysummary] function
#' @param test List of formulas specifying statistical tests to perform,
#' e.g. \code{list(all_continuous() ~ "svy.t.test", all_categorical() ~ "svy.wald.test")}.
#' Options include
#' * `"svy.t.test"` for a t-test adapted to complex survey samples (cf. [survey::svyttest]),
#' * `"svy.wilcox.test"` for a Wilcoxon rank-sum test for complex survey samples (cf. [survey::svyranktest]),
#' * `"svy.kruskal.test"` for a Kruskal-Wallis rank-sum test for complex survey samples (cf. [survey::svyranktest]),
#' * `"svy.vanderwaerden.test"` for a van der Waerden's normal-scores test for complex survey samples (cf. [survey::svyranktest]),
#' * `"svy.median.test"` for a Mood's test for the median for complex survey samples (cf. [survey::svyranktest]),
#' * `"svy.chisq.test"` for a Chi-squared test with Rao & Scott's second-order correction (cf. [survey::svychisq]),
#' * `"svy.adj.chisq.test"` for a Chi-squared test adjusted by a design effect estimate (cf. [survey::svychisq]),
#' * `"svy.wald.test"` for a Wald test of independence for complex survey samples (cf. [survey::svychisq]),
#' * `"svy.adj.wald.test"` for an adjusted Wald test of independence for complex survey samples (cf. [survey::svychisq]),
#' * `"svy.lincom.test"` for a test of independence using the exact asymptotic distribution for complex survey samples (cf. [survey::svychisq]),
#' * `"svy.saddlepoint.test"` for a test of independence using a saddlepoint approximation for complex survey samples (cf. [survey::svychisq]),
#'
#' Tests default to `"svy.wilcox.test"` for continuous variables and `"svy.chisq.test"`
#' for categorical variables.
#' @param ... Not used
#' @inheritParams tbl_regression
#' @inheritParams tbl_svysummary
#' @family tbl_svysummary tools
#' @export
#' @importFrom survey svyttest svyranktest svychisq
#' @return A `tbl_svysummary` object
#' @author Joseph Larmarange
#' @examples
#' # A simple weighted dataset
#' d <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
#' tbl_svysummary(d, by = "Survived", include = -Freq) %>% add_p()
#'
#' # A dataset with a complex design
#' data(api, package = "survey")
#' d_clust <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#' tbl_svysummary(d_clust, include = c("cname", "api00", "api99", "both"), by = "both") %>%
#'   add_p()
#' tbl_svysummary(d_clust, include = c("cname", "api00", "api99", "both"), by = "both") %>%
#'   add_p(test = list(all_continuous() ~ "svy.t.test", all_categorical() ~ "svy.wald.test"))

add_p.tbl_svysummary <- function(x, test = NULL, pvalue_fun = NULL,
                                 include = everything(), ...) {
  # checking for survey package ------------------------------------------------
  assert_package("survey", "add_p.tbl_svysummary")

  # setting defaults from gtsummary theme --------------------------------------
  test <- test %||%
    get_theme_element("add_p.tbl_svysummary-arg:test") %||%
    get_theme_element("add_p.tbl_summary-arg:test")
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("add_p.tbl_svysummary-arg:pvalue_fun") %||%
    get_theme_element("add_p.tbl_summary-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun")


  # converting bare arguments to string ----------------------------------------
  include <- var_input_to_string(data = select(x$inputs$data$variables, any_of(x$meta_data$variable)),
                                 select_input = !!rlang::enquo(include),
                                 arg_name = "include")

  # setting defaults -----------------------------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    getOption("gtsummary.pvalue_fun", default = style_pvalue)
  if (!rlang::is_function(pvalue_fun)) {
    stop(paste0(
      "'pvalue_fun' is not a valid function.  Please pass only a function\n",
      "object. For example,\n\n",
      "'pvalue_fun = function(x) style_pvalue(x, digits = 2)'"
    ), call. = FALSE)
  }

  # checking that input x has a by var
  if (is.null(x$df_by)) {
    stop(paste0(
      "Cannot add comparison when no 'by' variable ",
      "in original `tbl_svysummary()` call"
    ), call. = FALSE)
  }

  # test -----------------------------------------------------------------------
  # parsing into a named list
  test <- tidyselect_to_list(
    select(x$inputs$data$variables, any_of(x$meta_data$variable)),
    test, .meta_data = x$meta_data, arg_name = "test"
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
    mutate(
      # assigning statistical test to perform
      stat_test = assign_test(
        data = x$inputs$data,
        var = .data$variable,
        var_summary_type = .data$summary_type,
        by_var = x$by,
        test = test,
        group = NULL,
        env = caller_env,
        assign_test_one_fun = assign_test_one_survey
      ),
      # calculating pvalue
      test_result = calculate_pvalue(
        data = x$inputs$data,
        variable = .data$variable,
        by = x$by,
        test = .data$stat_test,
        type = .data$summary_type,
        group = NULL,
        include = include
      ),
      # grabbing p-value and test label from test_result
      p.value = map_dbl(
        .data$test_result,
        ~ pluck(.x, "p") %||% switch(is.numeric(.x), .x[1]) %||% NA_real_
      ),
      stat_test_lbl = map_chr(
        .data$test_result,
        ~ pluck(.x, "test") %||% NA_character_
      )
    ) %>%
    select(-.data$test_result)

  add_p_merge_p_values(x, meta_data, pvalue_fun)
}
