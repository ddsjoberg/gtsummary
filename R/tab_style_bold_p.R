#' Bold significant p-values
#'
#' Bold p-values in tables create by gtsummary
#'
#' @param x an object created using gtsummary functions
#' @param ... further arguments passed to other methods.
#' @author Daniel Sjoberg
#' @export

tab_style_bold_p <- function(x, ...) UseMethod("tab_style_bold_p")


#' Bold significant p-values
#'
#' Bold p-values in tables created by \code{\link{tbl_summary}}
#'
#' @param x an object created using `tbl_summary` function
#' @param t Determines the threshold below which p-values will be bold. Default is 0.05.
#' @param q logical argument. When TRUE will bold the q-value column rather than the p-values
#' @param ... not used
#' @author Daniel Sjoberg
#' @export
tab_style_bold_p.tbl_summary <- function(x, t = 0.05, q = FALSE, ...) {

  # checking that add_comparison has been previously run
  if(is.null(x$call_list$add_comparison)) {
    stop("Before bolding p-values, run add_comparison() to calculate the p-values")
  }
  # checking that add_q has been previously run if bolding q-values
  if(q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before bolding q-values, run add_q() to calculate the q-values")
  }

  # storing column names and gt_call name
  col_name = ifelse(q == FALSE, 'pvalue', 'qvalue')
  gt_call_name = glue("tab_style_bold_{ifelse(q == FALSE, 'p', 'q')}")

  # returning threshold for bolding
  x[[glue("{col_name}_bold_t")]] <- t
  # adding p-value formatting
  x[["gt_calls"]][[gt_call_name]] <- glue(
    "tab_style(style = cells_styles(text_weight = 'bold'), ",
    "locations = cells_data(columns = vars({col_name}),",
    "rows = {col_name} <= x${col_name}_bold_t))"
  )

  x
}

#' Bold significant p-values
#'
#' Bold p-values in tables created by \code{\link{tbl_regression}}
#'
#' @param x an object created using `tbl_regression` function
#' @param t Determines the threshold below which p-values will be bold. Default is 0.05.
#' @param ... not used
#' @author Daniel Sjoberg
#' @export
tab_style_bold_p.tbl_regression <- function(x, t = 0.05, ...) {

  # returning threshold for bolding
  x[[glue("pvalue_bold_t")]] <- t
  # adding p-value formatting
  x[["gt_calls"]][["tab_style_bold_pvalue"]] <- glue(
    "tab_style(style = cells_styles(text_weight = 'bold'), ",
    "locations = cells_data(columns = vars(pvalue),",
    "rows = pvalue <= x$pvalue_bold_t))"
  )

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
#' @author Daniel Sjoberg
#' @export
tab_style_bold_p.tbl_uvregression <- function(x, t = 0.05, q = FALSE, ...) {

  # checking that add_q has been previously run if bolding q-values
  if(q == TRUE & is.null(x$call_list$add_q)) {
    stop("Before bolding q-values, run add_q() to calculate the q-values")
  }

  # storing column names and gt_call name
  col_name = ifelse(q == FALSE, 'pvalue', 'qvalue')
  gt_call_name = glue("fmt_bold_{ifelse(q == FALSE, 'p', 'q')}")

  # returning threshold for bolding
  x[[glue("{col_name}_bold_t")]] <- t
  # adding p-value formatting
  x[["gt_calls"]][[gt_call_name]] <- glue(
    "tab_style(style = cells_styles(text_weight = 'bold'), ",
    "locations = cells_data(columns = vars({col_name}),",
    "rows = {col_name} <= x${col_name}_bold_t))"
  )

  x
}
