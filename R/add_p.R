#' Adds p-values to gtsummary table
#'
#' @param x Object created from a gtsummary function
#' @param ... Additional arguments passed to other methods.
#' @author Daniel D. Sjoberg
#' @seealso [add_p.tbl_summary], [add_p.tbl_cross]
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
#' project-level startup file, '.Rprofile'.  The default confidence level can
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
#' add_p_ex1 <-
#'   trial[c("age", "grade", "response", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p()
#'
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

  # converting bare arguments to string ----------------------------------------
  group <- var_input_to_string(data = x$inputs$data,
                               select_input = !!rlang::enquo(group),
                               arg_name = "group", select_single = TRUE)
  include <- var_input_to_string(data = x$inputs$data,
                                 select_input = !!rlang::enquo(include),
                                 arg_name = "include")
  exclude <- var_input_to_string(data = x$inputs$data,
                                 select_input = !!rlang::enquo(exclude),
                                 arg_name = "exclude")

  # group argument -------------------------------------------------------------
  if (!is.null(group)) {
    # checking group is in the data frame
    if (!group %in% x$meta_data$variable) {
      stop(glue("'{group}' is not a column name in the input data frame."), call. = FALSE)
    }
    # dropping group variable from table_body and meta_data
    x$table_body <- x$table_body %>% filter(.data$variable != group)
    x$meta_data <- x$meta_data %>% filter(.data$variable != group)
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
    x$inputs$data, test,
    .meta_data = x$meta_data, arg_name = "test"
  )

  if (!is.null(test)) {
    # checking that all inputs are named
    if ((names(test) %>%
         purrr::discard(. == "") %>%
         length()) != length(test)) {
      stop(glue(
        "Each element in 'test' must be named. ",
        "For example, 'test = list(age = \"t.test\", ptstage = \"fisher.test\")'"
      ), call. = FALSE)
    }
  }

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.", call. = FALSE)
  }

  # Getting p-values only for included variables
  include <- include %>% setdiff(exclude)


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
        group = group
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
        ~ pluck(.x, "p") %||% NA_real_
      ),
      stat_test_lbl = map_chr(
        .data$test_result,
        ~ pluck(.x, "test") %||% NA_character_
      )
    ) %>%
    select(-.data$test_result)

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
  x <- modify_header_internal(x, p.value = "**p-value**")

  x$call_list <- c(x$call_list, list(add_p = match.call()))

  x
}

# function to create text for footnote
footnote_add_p <- function(meta_data) {
  meta_data$stat_test_lbl %>%
    keep(~ !is.na(.)) %>%
    unique() %>%
    paste(collapse = "; ") %>%
    paste0("Statistical tests performed: ", .)
}


#' Adds p-value to crosstab table
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
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
#' @section Example 1 Output:
#' \if{html}{\figure{add_p_cross_ex1.png}{options: width=70\%}}
#'
#' @section Example 2 Output:
#' \if{html}{\figure{add_p_cross_ex2.png}{options: width=60\%}}
add_p.tbl_cross <- function(x, test = NULL, pvalue_fun = NULL,
                            source_note = FALSE, ...) {

  # adding test name if supplied (NULL otherwise)
  input_test <- switch(!is.null(test),
                       rlang::expr(everything() ~ !!test))

  # running add_p to add the p-value to the output
  x <- expr(add_p.tbl_summary(x, test = !!input_test)) %>% eval()

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

    # adding source note
    if(is.null(pvalue_fun)) pvalue_fun <- function(x) style_pvalue(x, prepend_p = TRUE)

    x$list_output$source_note <-
      paste(test_name, pvalue_fun(discard(x$meta_data$p.value, is.na)), sep = ", ")
  }

  # return tbl_cross
  x[["call_list"]] <- list(x[["call_list"]], add_p = match.call())
  x
}
