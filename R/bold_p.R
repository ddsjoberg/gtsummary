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
#' tbl_sum_bold_p_ex <-
#'   trial[c("age", "grade", "response", "trt")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   bold_p(t = 0.65)
#'
#' tbl_lm_bold_p_ex <-
#'   glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   bold_p(t = 0.65)
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_sum_bold_p_ex.png}{options: width=60\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_lm_bold_p_ex.png}{options: width=50\%}}

bold_p <- function(x, t = 0.05, q = FALSE) {
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


  # update table_header --------------------------------------------------------
  # storing column name to bold
  col_name <- ifelse(q == FALSE, "p.value", "q.value")

  # modifying table_header with bold threshold
  x$table_header$bold <- case_when(
    x$table_header$column == col_name & is.na(x$table_header$bold) ~
      glue("{col_name} <= {t}") %>% as.character(),
    x$table_header$column == col_name & !is.na(x$table_header$bold) ~
      paste(x$table_header$bold, glue("{col_name} <= {t}"), sep = " | "),
    TRUE ~ x$table_header$bold
  )

  # returning results ----------------------------------------------------------
  x$call_list <- c(x$call_list, list(bold_p = match.call()))

  x
}
