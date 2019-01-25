#' Adds p-values to the output comparing values across groups
#'
#' @param x object with class `fmt_table1` from the \code{\link{fmt_table1}} function
#' @param test user defined list of statistical tests provided as a named
#' character vector with variables as names and test functions as values.,
#' e.g. \code{list(age = "t.test", ptstage = "fisher.test")}.
#' Options include "t.test" for a T-test,
#' "wilcox.test" for a Wilcoxon rank sum test,
#' "kruskal.test" for a Kruskal-Wallis rank sum test,
#' "chisq.test" for a Chi-squared test,
#' "fisher.test" for a Fisher's exact test,
#' and "re" for a random intercept model to account for clustered data.
#' For "re" to be used "id" must also be specified in the function call.
#' @param pvalue_fun function for rounding/formatting p-values.  Default is \code{\link{fmt_pvalue}}.
#' The function must have a single input (the numeric, exact p-value),
#' and return a string that is the rounded/formatted p-value (e.g.
#' \code{pvalue_fun = function(x) fmt_pvalue(x, digits = 2)} or equivalently,
#'  \code{purrr::partial(fmt_pvalue, digits = 2)}).
#' @param id Character vector of an ID or grouping variable.  Summary statistics
#' will not be printed for this column, but they may be used in subsequent
#' functions. For example, the ID column may be used in `add_comparison()` to
#' include p-values with correlated data. Default is the `id = ` input from \code{\link{fmt_table1}}
#' @export
#' @examples
#' trial %>% fmt_table1(by = "trt") %>% add_comparison()
add_comparison <- function(x, test = NULL, pvalue_fun = fmt_pvalue, id = x$inputs$id) {
  # checking that input is class fmt_table1
  if (class(x) != "fmt_table1") stop("x must be class 'fmt_table1'")
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) stop("Cannot add comparison when no 'by' variable in original fmt_table1")

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    dplyr::mutate_(
      # assigning statistical test to perform
      stat_test = ~ assign_test(
        data = x$inputs$data,
        var = .variable,
        var_summary_type = .summary_type,
        by_var = x$inputs$by,
        test = test,
        id = id
      ),
      # calculating pvalue
      pvalue_exact = ~ calculate_pvalue(
        data = x$inputs$data,
        variable = .variable,
        by = x$inputs$by,
        test = stat_test,
        type = .summary_type,
        id = id
      ),
      # formatting pvalue
      pvalue = ~ pvalue_fun(pvalue_exact)
    )

  # stacking p-values and header rows
  pvalue_header <- create_header(pvalue = c("p-value", ""))
  pvalue_column <-
    dplyr::bind_rows(
      dplyr::bind_cols(pvalue_header$row_type, pvalue_header$pvalue),
      meta_data %>%
        dplyr::select(dplyr::one_of(c(".variable", "pvalue"))) %>%
        dplyr::mutate_(row_type = ~"label")
    )

  table1 <-
    x$table1 %>%
    dplyr::left_join(
      pvalue_column,
      by = c(".variable", "row_type")
    )

  x$table1 <- table1
  x$meta_data <- meta_data
  x$call_list <- c(x$call_list, list(add_comparison = match.call()))

  return(x)
}
