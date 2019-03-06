#' Add a column of q values to account for
#' multiple comparisons
#'
#' @param x `tbl_summary` or `tbl_uvregression` object
#' @param ... further arguments passed to or from other methods.
#' @author Esther Drill
#' @export
add_q <- function(x, ...) UseMethod("add_q")

#' Add a column of q values to `tbl_summary` object to account for
#' multiple comparisons in Rmarkdown
#'
#' The adjustments to the p-values is performed with
#' `stats::`\code{\link[stats]{p.adjust}}.  The default method for correction
#' is false discovery rate (`method = "fdr"`)
#'
#' @param x `tbl_summary` object
#' @param method character argument.  Methods from
#' `stats::`\code{\link[stats]{p.adjust}} are accepted.  Default is `method = 'fdr'`.
#' @param pvalue_fun function for rounding/formatting p-values.  Default is \code{\link{style_pvalue}}.
#' @param ...	further arguments passed to or from other methods
#' @author Esther Drill
#' @export
#' @examples
#' trial %>%
#'   tbl_summary(by = "trt") %>%
#'   add_comparison() %>%
#'   add_q()
add_q.tbl_summary <- function(x, method = "fdr", pvalue_fun = style_pvalue, ...) {

  # This adjusts p-values for multiple testing. Default method is fdr.
  if (!("add_comparison" %in% names(x$call_list))) {
    stop("There are no p-values yet. You need to use the function add_comparison()
    after tbl_summary() and before add_q()")
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate_(
      qvalue = ~ p.adjust(pvalue, method = method)
    )

  # adding q value to summary table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "qvalue")) %>%
        mutate_(row_type = ~"label"),
      by = c("variable", "row_type")
    )

  # keep track of what functions have been called
  x$call_list <- c(x$call_list, list(add_q = match.call()))

  x$qvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt:qvalue"]] <-
    "fmt(columns = vars(qvalue), rows = !is.na(qvalue), fns = x$qvalue_fun)"
  # column headers
  x[["gt_calls"]][["cols_label:qvalue"]] <-
    "cols_label(qvalue = md('**q-value**'))"

  # Returns the table 1 object
  return(x)
}


#' Add a column of q values to `tbl_uvregression` object to account for
#' multiple comparisons
#'
#' The adjustments to the p-values is performed with
#' `stats::`\code{\link[stats]{p.adjust}}.  The default method for correction
#' is false discovery rate (`"fdr"`)
#'
#' @param x `tbl_uvregression` object
#' @param method character argument.  Methods from
#' `stats::`\code{\link[stats]{p.adjust}} are accepted.  Default is `method = 'fdr'`.
#' @param pvalue_fun function for rounding/formatting p-values.  Default is \code{\link{style_pvalue}}.
#' @param ...	further arguments passed to or from other methods
#' @author Esther Drill
#' @export
#' @examples
#' trial %>%
#'   tbl_uvregression(
#'     method = "lm",
#'     y = "age"
#'   ) %>%
#'   add_global() %>%
#'   add_q()
add_q.tbl_uvregression <- function(x, method = "fdr", pvalue_fun = style_pvalue, ...) {

  # This adjusts p-values for multiple testing but only when the global approach is used.
  # Default method is fdr.
  if (!("pvalue_global" %in% colnames(x$meta_data))) {
    stop("You need global p-values first. Use the function add_global() after
    tbl_uvregression() and before add_q()")
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate_(
      qvalue_global = ~ p.adjust(pvalue_global, method = method)
    )

  # adding q value to display table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "qvalue_global")) %>%
        set_names(c("variable", "qvalue")) %>%
        mutate_(row_type = ~"label"),
      by = c("variable", "row_type")
    )

  x$call_list <- c(x$call_list, list(add_q = match.call()))

  x$qvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt:qvalue"]] <-
    "fmt(columns = vars(qvalue), rows = !is.na(qvalue), fns = x$qvalue_fun)"
  # column headers
  x[["gt_calls"]][["cols_label:qvalue"]] <-
    "cols_label(qvalue = md('**q-value**'))"

  return(x)
}
