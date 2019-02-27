#' Add a column of q values to objects to account for
#' multiple comparisons
#'
#' @param x `fmt_table1` or `fmt_uni_regression` object
#' @param ... further arguments passed to or from other methods.
#' @export
add_q <- function(x, ...) UseMethod("add_q")

#' Add a column of q values to `fmt_table1` object to account for
#' multiple comparisons in Rmarkdown
#'
#' The adjustments to the p-values is performed with
#' `stats::`\code{\link[stats]{p.adjust}}.  The default method for correction
#' is false discovery rate (`"fdr"`)
#'
#' @param x `table1` object
#' @param method character argument.  Methods from
#' `stats::`\code{\link[stats]{p.adjust}} are accepted.  Default is `method = fdr`.
#' @param pvalue_fun function for rounding/formatting p-values.  Default is \code{\link{fmt_pvalue}}.
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' trial %>% fmt_table1(by = "trt") %>% add_comparison() %>% add_q()
add_q.fmt_table1 <- function(x, method = "fdr", pvalue_fun = fmt_pvalue, ...) {

  # This adjusts p-values for multiple testing. Default method is fdr.
  if (!("add_comparison" %in% names(x$call_list))) {
    stop("There are no p-values yet. You need to use the function add_comparison()
    after fmt_table1() and before add_q()")
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    dplyr::mutate_(
      qvalue_exact = ~ p.adjust(pvalue_exact, method = method),
      qvalue = ~ pvalue_fun(qvalue_exact)
    )

  # adding q value to table1
  x$table1 <-
    x$table1 %>%
    dplyr::left_join(
      x$meta_data %>%
        dplyr::select(c(".variable", "qvalue")) %>%
        dplyr::mutate_(row_type = ~"label"),
      by = c(".variable", "row_type")
    ) %>%
    dplyr::mutate_(
      qvalue = ~ ifelse(row_type == "header2", "q-value", qvalue)
    )

  # keep track of what functions have been called
  x$call_list <- c(x$call_list, list(add_q = match.call()))

  # Returns the table 1 object
  return(x)
}


#' Add a column of q values to `fmt_uni_regression` object to account for
#' multiple comparisons
#'
#' The adjustments to the p-values is performed with
#' `stats::`\code{\link[stats]{p.adjust}}.  The default method for correction
#' is false discovery rate (`"fdr"`)
#'
#' @param x `table1` object
#' @param method character argument.  Methods from
#' `stats::`\code{\link[stats]{p.adjust}} are accepted.  Default is `method = fdr`.
#' @param ...	further arguments passed to or from other methods
#' @export
#' @examples
#' lm(mpg ~ hp + am, mtcars) %>%
#'   fmt_regression() %>%
#'   bold_p()
#'
#' trial %>%
#'   fmt_uni_regression(
#'     method = "lm",
#'     y = "age"
#'   ) %>%
#'   add_global() %>%
#'   add_q()
add_q.fmt_uni_regression <- function(x, method = "fdr", ...) {

  # This adjusts p-values for multiple testing but only when the global approach is used.
  # Default method is fdr.
  if (!("global_pvalue" %in% colnames(x$meta_data))) {
    stop("You need global p-values first. Use the function add_global() after
    fmt_uni_regression() and before add_q()")
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    dplyr::mutate_(
      qvalue_exact = ~ p.adjust(global_pvalue_exact, method = method),
      qvalue = ~ x$inputs$pvalue_fun(qvalue_exact)
    )

  # adding q value to table1
  x$model_tbl <-
    x$model_tbl %>%
    dplyr::left_join(
      x$meta_data %>%
        dplyr::select(c("variable", "qvalue")) %>%
        dplyr::mutate_(row_type = ~"label"),
      by = c("variable", "row_type")
    ) %>%
    dplyr::mutate_(
      qvalue = ~ ifelse(row_type == "header1", "q-value", qvalue)
    )

  x$call_list <- c(x$call_list, list(add_q = match.call()))
  return(x)
}
