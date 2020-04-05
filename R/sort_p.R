#' Sort variables in table by ascending p-values
#'
#' Sort tables created by gtsummary by p-values
#'
#' @param x An object created using gtsummary functions
#' @param q Logical argument. When `TRUE` will sort by the q-value column
#' @author Karissa Whiting
#' @export
#' @examples
#' tbl_sum_sort_p_ex <-
#'   trial[c("age", "grade", "response", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   sort_p()
#'
#' tbl_lm_sort_p_ex <-
#'   glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   sort_p()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_sum_sort_p_ex.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_lm_sort_p_ex.png}{options: width=50\%}}

sort_p <- function(x, q = FALSE) {
  # checking inputs ------------------------------------------------------------
  # checking class of x
  if (!inherits(x, "gtsummary")) {
    stop("`x=` must be a gtsummary obejct.", call. = FALSE)
  }

  # checking input table has a p.value column
  if (q == FALSE && !"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
         call. = FALSE)
  }

  # checking input table has a q.value column
  if (q == TRUE && !"q.value" %in% names(x$table_body)) {
    stop("There is no q-value column. `x$table_body` must have a column called 'q.value'",
         call. = FALSE)
  }

  # update table_body --------------------------------------------------------
  # storing column name to bold
  sort_var <- ifelse(q == FALSE, "p.value", "q.value")

  # sorting table by p.value or q.value
  x$table_body <-
    x$table_body %>%
    group_by(.data$variable) %>%
    mutate(sort_col = min(.data[[sort_var]], na.rm = TRUE)) %>%
    arrange(.data$sort_col) %>%
    select(-.data$sort_col) %>%
    ungroup()

  # returning results ----------------------------------------------------------
  x$call_list <- c(x$call_list, list(sort_p = match.call()))

  x
}
