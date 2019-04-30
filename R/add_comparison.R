#' Adds p-values to the output comparing values across groups
#'
#' @param x object with class `tbl_summary` from the [tbl_summary] function
#' @param test user defined list of statistical tests provided as a named
#' character vector with variables as names and test functions as values.,
#' e.g. \code{list(age = "t.test", ptstage = "fisher.test")}. Use the names
#' `..continuous..` and `..categorical..` to specify a test for all variables
#' of that type (`..categorical..` is applied to both categorical and
#' dichotomous variables).
#' Options include "t.test" for a t-test,
#' "wilcox.test" for a Wilcoxon rank-sum test,
#' "kruskal.test" for a Kruskal-Wallis rank-sum test,
#' "chisq.test" for a Chi-squared test,
#' "fisher.test" for a Fisher's exact test,
#' and "lme4" for a random intercept model to account for clustered data.
#' For "lme4" to be used "group" must also be specified in the [tbl_summary] call.
#' @inheritParams tbl_regression
#' @inheritParams tbl_summary
#' @family tbl_summary tools
#' @export
#' @author Emily C. Zabor, Daniel D. Sjoberg
#' @examples
#' add_comp_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, response, trt) %>%
#'   tbl_summary(by = "trt") %>%
#'   add_comparison()
#'
#' @section Example Output:
#' \if{html}{\figure{add_comp_ex1.png}{options: width=60\%}}
#'
add_comparison <- function(x, test = NULL, pvalue_fun = style_pvalue,
                           group = x$inputs$group) {
  # checking that input is class tbl_summary
  if (class(x) != "tbl_summary") stop("x must be class 'tbl_summary'")
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    stop("Cannot add comparison when no 'by' variable in original tbl_summary() call")
  }

  # test -----------------------------------------------------------------------
  if (!is.null(test)) {
    # checking that all inputs are named
    if ((names(test) %>% purrr::discard(. == "") %>% length()) != length(test)) {
      stop(glue(
        "Each element in 'test' must be named. ",
        "For example, 'test = list(age = \"t.test\", ptstage = \"fisher.test\")'"
      ))
    }
  }

  # checking pvalue_fun are functions
  if(!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.")
  }

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    mutate(
      # assigning statistical test to perform
      stat_test = assign_test(
        data = x$inputs$data,
        var = .data$variable,
        var_summary_type = .data$summary_type,
        by_var = x$inputs$by,
        test = test,
        group = group
      ),
      # calculating pvalue
      p.value = calculate_pvalue(
        data = x$inputs$data,
        variable = .data$variable,
        by = x$inputs$by,
        test = .data$stat_test,
        type = .data$summary_type,
        group = group
      )
    )

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
  x$pvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt_pvalue"]] <-
    "fmt(columns = vars(p.value), rows = !is.na(p.value), fns = x$pvalue_fun)" %>%
    glue()
  # column headers
  x[["gt_calls"]][["cols_label_pvalue"]] <-
    "cols_label(p.value = md('**p-value**'))" %>%
    glue()

  x$meta_data <- meta_data
  x$call_list <- c(x$call_list, list(add_comparison = match.call()))

  # adding footnote listing statistics presented in table
  x[["gt_calls"]][["footnote_add_comparison"]] <- glue(
    'tab_footnote(',
    'footnote = "{footnote_add_comparison(meta_data)}",',
    'locations = cells_column_labels(',
    'columns = vars(p.value))',
    ')'
  )

  x
}

# creates a tibble linking test names to labels
stat_test_names <- tibble::tribble(
  ~stat_test, ~stat_test_label,
  "t.test", "t-test",
  "fisher.test", "Fisher's exact test",
  "wilcox.test", "Wilcoxon rank-sum test",
  "kruskal.test", "Kruskal-Wallis test",
  "chisq.test", "chi-square test of independence",
  "lme4", "mixed-effects regression model with random intercept"
)

# function to create text for footnote
footnote_add_comparison <- function(meta_data) {
  meta_data %>%
    select("stat_test") %>%
    distinct() %>%
    left_join(stat_test_names, by = "stat_test") %>%
    pull("stat_test_label") %>%
    paste(collapse = "; ") %>%
    paste0("Statistical tests performed: ", .)
}
