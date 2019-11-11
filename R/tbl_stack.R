#' Stacks two or more gtsummary objects
#'
#' Assists in patching together more complex tables. `tbl_stack()` appends two
#' or more `tbl_regression`, `tbl_summary`, or `tbl_merge` objects.
#' {gt} attributes from the first regression object are utilized for output
#' table.
#'
#' @param tbls List of gtsummary objects
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @seealso [tbl_merge]
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_stack` object
#' @examples
#' # Example 1 - stacking two tbl_regression objects
#' t1 <-
#'   glm(response ~ trt, trial, family = binomial) %>%
#'   tbl_regression(
#'     exponentiate = TRUE,
#'     label = list(vars(trt) ~ "Treatment (unadjusted)")
#'   )
#'
#' t2 <-
#'   glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
#'   tbl_regression(
#'     include = "trt",
#'     exponentiate = TRUE,
#'     label = list(vars(trt) ~ "Treatment (adjusted)")
#'   )
#'
#' tbl_stack_ex1 <- tbl_stack(list(t1, t2))
#'
#' # Example 2 - stacking two tbl_merge objects
#' library(survival)
#' t3 <-
#'   coxph(Surv(ttdeath, death) ~ trt, trial) %>%
#'   tbl_regression(
#'     exponentiate = TRUE,
#'     label = list(vars(trt) ~ "Treatment (unadjusted)")
#'   )
#'
#' t4 <-
#'   coxph(Surv(ttdeath, death) ~ trt + grade + stage + marker, trial) %>%
#'   tbl_regression(
#'     include = "trt",
#'     exponentiate = TRUE,
#'     label = list(vars(trt) ~ "Treatment (adjusted)")
#'   )
#'
#'
#' # first merging, then stacking
#' row1 <- tbl_merge(list(t1, t3), tab_spanner = c("Tumor Response", "Death"))
#' row2 <- tbl_merge(list(t2, t4))
#' tbl_stack_ex2 <-
#'   tbl_stack(list(row1, row2))
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_stack_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_stack_ex2.png}{options: width=80\%}}

tbl_stack <- function(tbls) {
  # input checks ---------------------------------------------------------------
  # class of tbls
  if (!"list" %in% class(tbls)) {
    stop("Expecting 'tbls' to be a list, e.g. 'tbls = list(tbl1, tbl2)'")
  }

  # checking all inputs are class tbl_uvregression, tbl_regression, tbl_summary, or tbl_merge
  if (!map_chr(tbls, class) %in% c("tbl_regression", "tbl_uvregression",
                                   "tbl_summary", "tbl_merge") %>% any()) {
    stop("All objects in 'tbls' must be class 'tbl_regression',
         'tbl_uvregression', 'tbl_summary', or 'tbl_merge'")
  }

  # printing message if stacking tbl_summary and regression object
  if ("tbl_summary" %in% map_chr(tbls, class) &&
      any(c("tbl_regression", "tbl_uvregression") %in% map_chr(tbls, class))) {
    message(paste(
      "You are stacking a gtsummary regression table and a summary table,",
      "which is not recommended. Consider revising the format of your table."
    ))
  }

  # at least two objects must be passed
  tbls_length <- length(tbls)
  if (tbls_length < 2L) stop("Supply 2 or more gtsummary regression objects to 'tbls ='")

  # checking if there are multiple input types
  if (map_chr(tbls, class) %>% unique() %>% length() > 1) {
    message("Multiple gtsummary object classes detected. Displayed results default to first input class type.")
  }

  # stacking tables ------------------------------------------------------------
  results <- tbls[[1]][names(tbls[[1]]) %>% intersect(c(
    "inputs", "gt_calls", "kable_calls", "estimate_funs",
    "pvalue_funs", "qvalue_funs",
    "pvalue_fun", "qvalue_fun",
    "table_header", "tbls"
  ))]

  results$table_body <-
    map_dfr(
      tbls,
      ~ pluck(.x, "table_body")
    )

  # returning results ----------------------------------------------------------
  results$call_list <- list(tbl_stack = match.call())
  results$tbl_regression_list <- tbls

  class(results) <- "tbl_stack"
  return(results)
}
