#' Bold significant p-values
#'
#' Bold p-values below a chosen threshold (e.g. <0.05)
#' in tables created by `tbl_summary`.
#'
#' @param x an object created using gtsummary functions
#' @param ... further arguments passed to other methods.
#' @author Daniel D. Sjoberg, Esther Drill
#' @seealso \code{\link{bold_p.tbl_summary}},
#' \code{\link{bold_p.tbl_regression}},
#' \code{\link{bold_p.tbl_uvregression}}
#' @export
bold_p <- function(x, ...) UseMethod("bold_p")


#' Bold significant p-values
#'
#' Bold p-values in tables created by \code{\link{tbl_summary}}
#'
#' @param x an object created using `tbl_summary` function
#' @param t threshold below which p-values will be bold. Default is 0.05.
#' @param q logical argument. When TRUE will bold the q-value column rather than the p-values
#' @param ... not used
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg, Esther Drill
#' @examples
#' tbl_sum_bold_p_ex <-
#'   trial %>%
#'   dplyr::select(age, grade, response, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   bold_p()
#' @section Example Output:
#' \if{html}{\figure{tbl_sum_bold_p_ex.png}{options: width=50\%}}
#' @export
bold_p.tbl_summary <- function(x, t = 0.05, q = FALSE, ...) {

  # checking that add_p has been previously run
  if (is.null(x$call_list$add_p)) {
    stop("Before p-values are bolded, run add_p() to calculate the p-values")
  }
  # checking that add_q has been previously run if bold q-values
  if (q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before q-values are bolded, run add_q() to calculate the q-values")
  }

  # storing column names and gt_call name
  col_name <- ifelse(q == FALSE, "p.value", "q.value")
  fun_name <- ifelse(q == FALSE, "pvalue_fun", "qvalue_fun")

  # modifying table_header with bold threshold
  x$table_header$bold <-
    ifelse(
      x$table_header$column == col_name,
      t, x$table_header$bold
    )

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  x$call_list <- c(x$call_list, list(bold_p = match.call()))

  x
}

#' Bold significant p-values
#'
#' Bold p-values below a chosen threshold (e.g. <.05) in tables
#' created by \code{\link{tbl_regression}}.
#'
#' @param x an object created using `tbl_regression` function
#' @param t Determines the threshold below which p-values will be bold. Default is 0.05.
#' @param ... not used
#' @author Daniel D. Sjoberg, Esther Drill
#' @family tbl_regression tools
#' @examples
#' tbl_lm_bold_p_ex <-
#'   glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   bold_p()
#' @section Example Output:
#' \if{html}{\figure{tbl_lm_bold_p_ex.png}{options: width=50\%}}
#' @export
#'
bold_p.tbl_regression <- function(x, t = 0.05, ...) {

  # modifying table_header with bold threshold
  x$table_header$bold <-
    ifelse(
      x$table_header$column == "p.value",
      t, x$table_header$bold
    )

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  # kable formatting -----------------------------------------------------------
  # replacing previous kable call, kable
  # x[["kable_calls"]][["fmt_pvalue"]] <- glue(
  #   "mutate(p.value = dplyr::case_when(",
  #   "p.value <= {t} ~ paste0('__', x$inputs$pvalue_fun(p.value), '__'), ",
  #   "TRUE ~ x$inputs$pvalue_fun(p.value)",
  #   "))"
  # )

  x$call_list <- c(x$call_list, list(bold_p = match.call()))

  x
}

#' Bold significant p-values
#'
#' Bold p-values below a chosen threshold (e.g. <.05) in tables created by \code{\link{tbl_uvregression}}
#'
#' @param x an object created using `tbl_uvregression` function
#' @param t Determines the threshold below which p-values will be bold. Default is 0.05.
#' @param q logical argument. When TRUE will bold the q-value column rather than the p-values
#' @param ... not used
#' @author Daniel D. Sjoberg, Esther Drill
#' @family tbl_uvregression tools
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
#'   bold_p(t = 0.25)
#' @section Example Output:
#' \if{html}{\figure{tbl_uvglm_bold_p_ex.png}{options: width=50\%}}

bold_p.tbl_uvregression <- function(x, t = 0.05, q = FALSE, ...) {

  # checking that add_q has been previously run if bold q-values
  if (q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before q-values are bolded, run add_q() to calculate the q-values")
  }

  col_name <- ifelse(q == FALSE, "p.value", "q.value")
  fun_name <- ifelse(q == FALSE, "pvalue_fun", "qvalue_fun")

  # modifying table_header with bold threshold
  x$table_header$bold <-
    ifelse(
      x$table_header$column == col_name,
      t, x$table_header$bold
    )

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  x$call_list <- c(x$call_list, list(bold_p = match.call()))

  x
}

#' Bold significant p-values
#'
#' Bold p-values below a chosen threshold (e.g. <.05) in tables created by \code{\link{tbl_stack}}
#'
#' @param x an object created using `tbl_stack` function
#' @param ... arguments passed to `bold_p.*()` method that m
#' atches the first object in the `tbl_stack`
#' @author Daniel D. Sjoberg
#' @family tbl_uvregression tools
#' @family tbl_regression tools
#' @export
#' @examples
#' t1 <- tbl_regression(lm(age ~ response, trial))
#' t2 <- tbl_regression(lm(age ~ grade, trial))
#'
#' bold_p_stack_ex <-
#'   tbl_stack(list(t1, t2)) %>%
#'   bold_p(t = 0.10)
#' @section Example Output:
#' \if{html}{\figure{bold_p_stack_ex.png}{options: width=50\%}}

bold_p.tbl_stack <- function(x, ...) {

  # assigning the class to be the same as the first stacked object
  class(x) <- class(x$tbl_regression_list[[1]])

  bold_p(x, ...)
}
