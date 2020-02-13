#' Add a column of q values to account for
#' multiple comparisons
#'
#' @param x `tbl_summary` or `tbl_uvregression` object
#' @param ... Additional arguments passed to other methods.
#' @author Esther Drill, Daniel D. Sjoberg
#' @seealso \code{\link{add_q.tbl_summary}}, \code{\link{add_q.tbl_uvregression}},
#' \code{\link{tbl_summary}}, \code{\link{tbl_uvregression}}
#' @export
add_q <- function(x, ...) UseMethod("add_q")

#' Add a column of q-values to account for multiple comparisons
#'
#' Adjustments to are p-values are performed with [stats::p.adjust].
#'
#' @param x `tbl_summary` object
#' @param method String indicating method to be used for p-value
#' adjustment. Methods from
#' [stats::p.adjust] are accepted.  Default is `method = 'fdr'`.
#' @inheritParams tbl_regression
#' @param ...	Additional arguments passed to or from other methods
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_summary tools
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_sum_q_ex <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   add_q()
#' @section Example Output:
#' \if{html}{\figure{tbl_sum_q_ex.png}{options: width=50\%}}

add_q.tbl_summary <- function(x, method = "fdr",
                              pvalue_fun = x$fmt_fun$p.value, ...) {

  # This adjusts p-values for multiple testing. Default method is fdr.
  if (!("add_p" %in% names(x$call_list))) {
    stop(glue(
      "There are no p-values yet. You need to use the function add_p(), ",
      "after tbl_summary() and before add_q()"
    ))
  }

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.")
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate(
      q.value = stats::p.adjust(.data$p.value, method = method)
    )

  # footnote text
  footnote_text <-
    add_q_method_lookup[add_q_method_lookup$method == method, ]$method_label

  # adding q value to summary table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "q.value")) %>%
        mutate(row_type = "label"),
      by = c("variable", "row_type")
    )

  x$table_header <-
    tibble(column = names(x$table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing() %>%
    # table_header_fmt(q.value = "x$qvalue_fun") %>%
    table_header_fmt_fun(q.value = pvalue_fun) %>%
    mutate(footnote = map2(
      .data$column, .data$footnote,
      function(x, y) {
        if (x == "q.value") {
          return(c(y, footnote_text))
        }
        return(y)
      }
    ))

  # adding  column header
  x <- modify_header_internal(x, q.value = "**q-value**")

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  # keep track of what functions have been called
  x$call_list <- c(x$call_list, list(add_q = match.call()))

  # returning qvalue method
  x$qvalue_method <- method

  # Returns the table 1 object
  return(x)
}


#' Add a column of q-values to account for multiple comparisons
#'
#' Adjustments to are p-values are performed with [stats::p.adjust].
#'
#' @param x `tbl_uvregression` object
#' @param method String indicating method to be used for p-value adjustment.
#' Methods from [stats::p.adjust] are accepted. Default is `method = 'fdr'`.
#' @inheritParams tbl_regression
#' @param ...	Additional arguments passed to or from other methods
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_uvregression tools
#' @export
#' @return A `tbl_uvregression` object
#' @examples
#' tbl_uvr_q_ex <-
#'   trial[c("age", "marker", "grade", "response")] %>%
#'   tbl_uvregression(
#'     method = lm,
#'     y = age
#'   ) %>%
#'   add_global_p() %>%
#'   add_q()
#' @section Example Output:
#' \if{html}{\figure{tbl_uvr_q_ex.png}{options: width=50\%}}

add_q.tbl_uvregression <- function(x, method = "fdr",
                                   pvalue_fun = x$fmt_fun$p.value, ...) {

  # This adjusts p-values for multiple testing but only when the
  # global approach is used. Default method is fdr.
  if (!("p.value_global" %in% colnames(x$meta_data))) {
    stop(glue(
      "You need global p-values first. Use the function add_global_p() after ",
      "tbl_uvregression() and before add_q()"
    ))
  }

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.")
  }

  # footnote text
  footnote_text <-
    add_q_method_lookup[add_q_method_lookup$method == method, ]$method_label

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate(
      q.value_global = stats::p.adjust(.data$p.value_global, method = method)
    )

  # adding q value to display table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "q.value_global")) %>%
        set_names(c("variable", "q.value")) %>%
        mutate(row_type = "label"),
      by = c("variable", "row_type")
    )

  x$table_header <-
    tibble(column = names(x$table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing() %>%
    # table_header_fmt(q.value = "x$qvalue_fun") %>%
    table_header_fmt_fun(q.value = pvalue_fun) %>%
    mutate(footnote = map2(
      .data$column, .data$footnote,
      function(x, y) {
        if (x == "q.value") {
          return(c(y, footnote_text))
        }
        return(y)
      }
    ))

  x <- modify_header_internal(x, q.value = "**q-value**")

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  x$call_list <- c(x$call_list, list(add_q = match.call()))

  # returning qvalue method
  x$qvalue_method <- method

  return(x)
}


# match method input to display name
add_q_method_lookup <-
  tibble::tibble(
    method = stats::p.adjust.methods
  ) %>%
  left_join(
    tibble::tribble(
      ~method, ~method_label,
      "holm", "Holm correction for multiple testing",
      "hochberg", "Hochberg correction for multiple testing",
      "hommel", "Hommel correction for multiple testing",
      "bonferroni", "Bonferroni correction for multiple testing",
      "BH", "Benjamini & Hochberg correction for multiple testing",
      "BY", "Benjamini & Yekutieli correction for multiple testing",
      "fdr", "False discovery rate correction for multiple testing",
      "none", "No correction for multiple testing"
    ),
    by = "method"
  ) %>%
  mutate(method_label = coalesce(method_label, method))
