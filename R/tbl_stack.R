#' Stacks two or more gtsummary regression objects
#'
#' Stacks two or more `tbl_regression` or `tbl_uvregression` objects.
#' {gt} attributes from the first regression object are utilized for output table.
#' If combining `tbl_stack()` and `tbl_merge`, merge first then stack.
#'
#' @param tbls list of gtsummary regression objects
#' @export
#' @examples
#' t1 <-
#'   glm(response ~ age, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE,
#'                  label = list(age = "Age, Unadjusted"))
#'
#' t2 <-
#'   glm(response ~ age + grade + stage + marker, trial, family = binomial) %>%
#'   tbl_regression(include = "age",
#'                  exponentiate = TRUE,
#'                  label = list(age = "Age, Adjusted"))
#'
#' tbl_stack_ex1 <- tbl_stack(list(t1, t2))

tbl_stack <- function(tbls) {
  # input checks ---------------------------------------------------------------
  # checking all inputs are class tbl_regression or tbl_uvregression
  if (!map_chr(tbls, class) %in% c("tbl_regression", "tbl_uvregression", "tbl_merge") %>% any()) {
    stop("All objects in 'tbls' must be class 'tbl_regression' or 'tbl_uvregression'")
  }

  # at least two objects must be passed
  tbls_length <- length(tbls)
  if (tbls_length < 1) stop("Supply 2 or more gtsummary regression objects to 'tbls ='")

  # stacking tables ------------------------------------------------------------
  results <- tbls[[1]][names(tbls[[1]]) %>% intersect(c("inputs", "gt_calls", "estimate_funs", "pvalue_funs", "qvalue_funs"))]

  results$table_body <-
    map_dfr(
      tbls,
      ~ pluck(.x, "table_body")
    )



  # returning results ----------------------------------------------------------
  results$call_list <- list(tbl_stack = match.call())
  results$tbl_regression_list = tbls

  class(results) <- "tbl_stack"
  return(results)
}
