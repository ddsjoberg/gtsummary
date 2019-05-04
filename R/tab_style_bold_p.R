#' Bold significant p-values
#'
#' Bold p-values in tables created by gtsummary
#'
#' @param x an object created using gtsummary functions
#' @param ... further arguments passed to other methods.
#' @author Daniel D. Sjoberg
#' @seealso \code{\link{tab_style_bold_p.tbl_summary}},
#' \code{\link{tab_style_bold_p.tbl_regression}},
#' \code{\link{tab_style_bold_p.tbl_uvregression}}
#' @export

tab_style_bold_p <- function(x, ...) UseMethod("tab_style_bold_p")


#' Bold significant p-values
#'
#' Bold p-values in tables created by \code{\link{tbl_summary}}
#'
#' @param x an object created using `tbl_summary` function
#' @param t threshold below which p-values will be bold. Default is 0.05.
#' @param q logical argument. When TRUE will bold the q-value column rather than the p-values
#' @param ... not used
#' @family tbl_summary
#' @author Daniel D. Sjoberg
#' @examples
#' tbl_sum_bold_p_ex <-
#'   trial %>%
#'   dplyr::select(age, grade, response, trt) %>%
#'   tbl_summary(by = "trt") %>%
#'   add_comparison() %>%
#'   tab_style_bold_p()
#' @section Example Output:
#' \if{html}{\figure{tbl_sum_bold_p_ex.png}{options: width=50\%}}
#' @export
tab_style_bold_p.tbl_summary <- function(x, t = 0.05, q = FALSE, ...) {

  # checking that add_comparison has been previously run
  if (is.null(x$call_list$add_comparison)) {
    stop("Before p-values are bolded, run add_comparison() to calculate the p-values")
  }
  # checking that add_q has been previously run if bold q-values
  if (q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before q-values are bolded, run add_q() to calculate the q-values")
  }

  # storing column names and gt_call name
  col_name <- ifelse(q == FALSE, "p.value", "q.value")
  gt_call_name <- glue("tab_style_bold_{ifelse(q == FALSE, 'p', 'q')}")

  # returning threshold for bold
  x[[glue("{col_name}_bold_t")]] <- t
  # adding p-value formatting
  x[["gt_calls"]][[gt_call_name]] <- glue(
    "tab_style(style = cells_styles(text_weight = 'bold'), ",
    "locations = cells_data(columns = vars({col_name}),",
    "rows = {col_name} <= x${col_name}_bold_t))"
  )

  x$call_list <- c(x$call_list, list(tab_style_bold_p = match.call()))

  x
}

#' Bold significant p-values
#'
#' Bold p-values in tables created by \code{\link{tbl_regression}}
#'
#' @param x an object created using `tbl_regression` function
#' @param t Determines the threshold below which p-values will be bold. Default is 0.05.
#' @param ... not used
#' @author Daniel D. Sjoberg
#' @family tbl_regression
#' @examples
#' tbl_lm_bold_p_ex <-
#'   glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   tab_style_bold_p()
#' @section Example Output:
#' \if{html}{\figure{tbl_lm_bold_p_ex.png}{options: width=50\%}}
#' @export
#'
tab_style_bold_p.tbl_regression <- function(x, t = 0.05, ...) {

  # returning threshold for bold
  x[[glue("pvalue_bold_t")]] <- t
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_bold_pvalue"]] <- glue(
    "tab_style(style = cells_styles(text_weight = 'bold'), ",
    "locations = cells_data(columns = vars(p.value),",
    "rows = p.value <= x$pvalue_bold_t))"
  )

  x$call_list <- c(x$call_list, list(tab_style_bold_p = match.call()))

  x
}

#' Bold significant p-values
#'
#' Bold p-values in tables created by \code{\link{tbl_uvregression}}
#'
#' @param x an object created using `tbl_uvregression` function
#' @param t Determines the threshold below which p-values will be bold. Default is 0.05.
#' @param q logical argument. When TRUE will bold the q-value column rather than the p-values
#' @param ... not used
#' @author Daniel D. Sjoberg
#' @family tbl_uvregression
#' @export
#' @examples
#' tbl_uvglm_bold_p_ex <-
#'   trial %>%
#'   dplyr::select(age, marker, response, grade) %>%
#'   tbl_uvregression(
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'   tab_style_bold_p(t = 0.25)
#' @section Example Output:
#' \if{html}{\figure{tbl_uvglm_bold_p_ex.png}{options: width=50\%}}

tab_style_bold_p.tbl_uvregression <- function(x, t = 0.05, q = FALSE, ...) {

  # checking that add_q has been previously run if bold q-values
  if (q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before q-values are bolded, run add_q() to calculate the q-values")
  }

  # storing column names and gt_call name
  col_name <- ifelse(q == FALSE, "p.value", "q.value")
  gt_call_name <- glue("fmt_bold_{ifelse(q == FALSE, 'p', 'q')}")

  # returning threshold for bold
  x[[glue("{col_name}_bold_t")]] <- t
  # adding p-value formatting
  x[["gt_calls"]][[gt_call_name]] <- glue(
    "tab_style(style = cells_styles(text_weight = 'bold'), ",
    "locations = cells_data(columns = vars({col_name}),",
    "rows = {col_name} <= x${col_name}_bold_t))"
  )

  x$call_list <- c(x$call_list, list(tab_style_bold_p = match.call()))

  x
}
