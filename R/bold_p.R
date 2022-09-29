#' Bold significant p-values or q-values
#'
#' Bold values below a chosen threshold (e.g. <0.05)
#' in a gtsummary tables.
#'
#' @param x Object created using gtsummary functions
#' @param t Threshold below which values will be bold. Default is 0.05.
#' @param q Logical argument. When TRUE will bold the q-value column rather
#' than the p-values. Default is `FALSE`.
#' @author Daniel D. Sjoberg, Esther Drill
#' @export
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' bold_p_ex1 <-
#'   trial[c("age", "grade", "response", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   bold_p(t = 0.65)
#'
#' # Example 2 ----------------------------------
#' bold_p_ex2 <-
#'   glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   bold_p(t = 0.65)
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "bold_p_ex1.png", width = "60")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "bold_p_ex2.png", width = "50")`
#' }}

bold_p <- function(x, t = 0.05, q = FALSE) {
  updated_call_list <- c(x$call_list, list(bold_p = match.call()))
  # checking inputs ------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # checking input table has a p.value column
  if (q == FALSE && !"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
      call. = FALSE
    )
  }

  # checking input table has a q.value column
  if (q == TRUE && !"q.value" %in% names(x$table_body)) {
    stop("There is no q-value column. `x$table_body` must have a column called 'q.value'",
      call. = FALSE
    )
  }


  # update table_styling -------------------------------------------------------
  # storing column name to bold
  col_name <- ifelse(q == FALSE, "p.value", "q.value")

  # modifying table_styling with bold threshold
  x <-
    modify_table_styling(
      x,
      columns = all_of(col_name),
      rows = !!expr(!!sym(col_name) <= !!t),
      text_format = "bold"
    )

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
