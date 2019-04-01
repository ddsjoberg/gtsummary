#' Add a column of q values to account for
#' multiple comparisons
#'
#' @param x `tbl_summary` or `tbl_uvregression` object
#' @param ... further arguments passed to or from other methods.
#' @author Esther Drill, Daniel D. Sjoberg
#' @seealso \code{\link{tbl_summary}}, \code{\link{tbl_regression}},
#' \code{\link{tbl_uvregression}}
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
#' @inheritParams tbl_regression
#' @param ...	further arguments passed to or from other methods
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_summary
#' @export
#' @examples
#' tbl_q <-
#'   trial %>%
#'   tbl_summary(by = "trt") %>%
#'   add_comparison() %>%
#'   add_q()
add_q.tbl_summary <- function(x, method = "fdr", pvalue_fun = x$pvalue_fun, ...) {

  # This adjusts p-values for multiple testing. Default method is fdr.
  if (!("add_comparison" %in% names(x$call_list))) {
    stop(glue(
      "There are no p-values yet. You need to use the function add_comparison(), ",
      "after tbl_summary() and before add_q()"
    ))
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate(
      qvalue = stats::p.adjust(.data$pvalue, method = method)
    )

  # adding q value to summary table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "qvalue")) %>%
        mutate(row_type = "label"),
      by = c("variable", "row_type")
    )

  # keep track of what functions have been called
  x$call_list <- c(x$call_list, list(add_q = match.call()))

  # footnote text
  footnote_text <-
    method %>%
    {filter(add_q_method_lookup, !!parse_expr(glue("method == '{.}'")))} %>%
    pull("method_label")

  x$qvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt:qvalue"]] <-
    "fmt(columns = vars(qvalue), rows = !is.na(qvalue), fns = x$qvalue_fun)"
  # column headers
  x[["gt_calls"]][["cols_label:qvalue"]] <-
    "cols_label(qvalue = md('**q-value**'))"
  # column headers abbreviations footnote
  x[["gt_calls"]][["footnote_q_method"]] = glue(
    "tab_footnote(",
    "  footnote = '{footnote_text}',",
    "  locations = cells_column_labels(",
    "    columns = vars(qvalue))",
    ")"
  )

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
#' @inheritParams tbl_regression
#' @param ...	further arguments passed to or from other methods
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_uvregression
#' @export
#' @examples
#' tbl_q <-
#'   trial %>%
#'   tbl_uvregression(
#'     method = lm,
#'     y = age
#'   ) %>%
#'   add_global() %>%
#'   add_q()
add_q.tbl_uvregression <- function(x, method = "fdr",
                                   pvalue_fun = x$inputs$pvalue_fun, ...) {

  # This adjusts p-values for multiple testing but only when the global approach is used.
  # Default method is fdr.
  if (!("pvalue_global" %in% colnames(x$meta_data))) {
    stop(glue(
      "You need global p-values first. Use the function add_global() after",
      "tbl_uvregression() and before add_q()"
    ))
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate(
      qvalue_global = stats::p.adjust(.data$pvalue_global, method = method)
    )

  # adding q value to display table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "qvalue_global")) %>%
        set_names(c("variable", "qvalue")) %>%
        mutate(row_type = "label"),
      by = c("variable", "row_type")
    )

  x$call_list <- c(x$call_list, list(add_q = match.call()))

  # footnote text
  footnote_text <-
    method %>%
    {filter(add_q_method_lookup, !!parse_expr(glue("method == '{.}'")))} %>%
    pull("method_label")

  x$qvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt:qvalue"]] <-
    "fmt(columns = vars(qvalue), rows = !is.na(qvalue), fns = x$qvalue_fun)" %>%
    glue()
  # column headers
  x[["gt_calls"]][["cols_label:qvalue"]] <-
    "cols_label(qvalue = md('**q-value**'))"%>%
    glue()
  # column headers abbreviations footnote
  x[["gt_calls"]][["footnote_q_method"]] = glue(
    "tab_footnote(",
    "footnote = '{footnote_text}',",
    "locations = cells_column_labels(",
    "columns = vars(qvalue))",
    ")"
  )

  return(x)
}


# match method input to display name
add_q_method_lookup <- tibble::tribble(
  ~method, ~method_label,
  "holm", "Holm correction for multiple testing",
  "hochberg", "Hochberg correction for multiple testing",
  "hommel", "Hommel correction for multiple testing",
  "bonferroni", "Bonferroni correction for multiple testing",
  "BH", "Benjamini & Hochberg correction for multiple testing",
  "BY", "Benjamini & Yekutieli correction for multiple testing",
  "fdr", "False discovery rate correction for multiple testing",
  "none", "No correction for multiple testing"
)
