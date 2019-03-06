#' Adds p-values to the output comparing values across groups
#'
#' @param x object with class `tbl_summary` from the \code{\link{tbl_summary}} function
#' @param test user defined list of statistical tests provided as a named
#' character vector with variables as names and test functions as values.,
#' e.g. \code{list(age = "t.test", ptstage = "fisher.test")}.
#' Options include "t.test" for a T-test,
#' "wilcox.test" for a Wilcoxon rank sum test,
#' "kruskal.test" for a Kruskal-Wallis rank sum test,
#' "chisq.test" for a Chi-squared test,
#' "fisher.test" for a Fisher's exact test,
#' and "re" for a random intercept model to account for clustered data.
#' For "re" to be used "group" must also be specified in the function call.
#' @param pvalue_fun function for rounding/formatting p-values.
#' Default is \code{\link{style_pvalue}}.
#' The function must have a single input (the numeric, exact p-value),
#' and return a string that is the rounded/formatted p-value (e.g.
#' \code{pvalue_fun = function(x) style_pvalue(x, digits = 2)} or equivalently,
#'  \code{purrr::partial(style_pvalue, digits = 2)}).
#' @param group Character vector of an ID or grouping variable.  Summary statistics
#' will not be printed for this column, but they may be used in subsequent
#' functions. For example, the group column may be used in `add_comparison()` to
#' include p-values with correlated data. Default is the `group = ` input
#' from \code{\link{tbl_summary}}
#' @export
#' @author Daniel Sjoberg
#' @examples
#' trial %>% tbl_summary(by = "trt") %>% add_comparison()
add_comparison <- function(x, test = NULL, pvalue_fun = style_pvalue, group = x$inputs$group) {
  # checking that input is class tbl_summary
  if (class(x) != "tbl_summary") stop("x must be class 'tbl_summary'")
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) stop("Cannot add comparison when no 'by' variable in original tbl_summary() call")

  # getting the test name and pvalue
  meta_data <-
    x$meta_data %>%
    mutate_(
      # assigning statistical test to perform
      stat_test = ~ assign_test(
        data = x$inputs$data,
        var = variable,
        var_summary_type = summary_type,
        by_var = x$inputs$by,
        test = test,
        group = group
      ),
      # calculating pvalue
      pvalue = ~ calculate_pvalue(
        data = x$inputs$data,
        variable = variable,
        by = x$inputs$by,
        test = stat_test,
        type = summary_type,
        group = group
      )
    )

  # creating pvalue column for table_body merge
  pvalue_column <-
    meta_data %>%
    select(c("variable", "pvalue")) %>%
    mutate_(row_type = ~"label")


  table_body <-
    x$table_body %>%
    dplyr::left_join(
      pvalue_column,
      by = c("variable", "row_type")
    )

  x$table_body <- table_body
  x$pvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt_pvalue"]] <-
    "fmt(columns = vars(pvalue), rows = !is.na(pvalue), fns = x$pvalue_fun)"
  # column headers
  x[["gt_calls"]][["cols_label_pvalue"]] <-
    "cols_label(pvalue = md('**p-value**'))"

  x$meta_data <- meta_data
  x$call_list <- c(x$call_list, list(add_comparison = match.call()))

  return(x)
}
