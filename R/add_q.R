#' Add a column of q-values to account for
#' multiple comparisons
#'
#' Adjustments to p-values are performed with [stats::p.adjust].
#'
#' @param x a `gtsummary` object
#' @param method String indicating method to be used for p-value
#' adjustment. Methods from
#' [stats::p.adjust] are accepted.  Default is `method = "fdr"`.
#' @inheritParams tbl_regression
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @export
#' @examples
#' tbl_sum_q_ex1 <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   add_q()
#'
#' tbl_uv_q_ex2 <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_uvregression(
#'     y = response,
#'     method = glm,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'     add_global_p() %>%
#'     add_q()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_sum_q_ex1.png}{options: width=65\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_uv_q_ex2.png}{options: width=60\%}}
add_q <- function(x, method = "fdr", pvalue_fun = NULL) {
  # checking inputs ------------------------------------------------------------
  # checking class of x
  if (!inherits(x, "gtsummary")) {
    stop("`x=` must be a gtsummary obejct.", call. = FALSE)
  }

  # checking input table has a p.value column
  if (!"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
         call. = FALSE)
  }

  # setting defaults -----------------------------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    filter(x$table_header, .data$column == "p.value") %>% pull(.data$fmt_fun) %>% pluck(1)

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.")
  }

  # perform multiple comparisons -----------------------------------------------
  x$table_body$q.value <- x$table_body$p.value %>% stats::p.adjust(method = method)

  # update table_header --------------------------------------------------------
  # footnote text
  footnote_text <-
    add_q_method_lookup[add_q_method_lookup$method == method, ]$method_label

  x$table_header <-
    tibble(column = names(x$table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing() %>%
    # table_header_fmt(q.value = "x$qvalue_fun") %>%
    table_header_fmt_fun(q.value = pvalue_fun) %>%
    mutate(
      footnote = ifelse(.data$column == "q.value",
                        footnote_text,
                        .data$footnote)
    )

  # adding  column header
  x <- modify_header_internal(x, q.value = "**q-value**")

  # return final object --------------------------------------------------------
  # adding call
  x$call_list <- c(x$call_list, list(add_q = match.call()))

  x
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
