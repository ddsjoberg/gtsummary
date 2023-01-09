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
#' @inheritParams add_global_p
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @export
#' @examplesIf broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE)
#' \donttest{
#' # Example 1 ----------------------------------
#' add_q_ex1 <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   add_q()
#'
#' # Example 2 ----------------------------------
#' add_q_ex2 <-
#'   trial[c("trt", "age", "grade", "response")] %>%
#'   tbl_uvregression(
#'     y = response,
#'     method = glm,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   ) %>%
#'   add_global_p() %>%
#'   add_q()
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_q_ex1.png", width = "65")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_q_ex2.png", width = "60")`
#' }}

add_q <- function(x, method = "fdr", pvalue_fun = NULL, quiet = NULL) {
  updated_call_list <- c(x$call_list, list(add_q = match.call()))
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking inputs ------------------------------------------------------------
  # checking class of x
  .assert_class(x, "gtsummary")

  # checking input table has a p.value column
  if (!"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
      call. = FALSE
    )
  }

  # setting defaults from gtsummary theme --------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    # defaults from theme
    get_theme_element("add_q-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    # default from p-value formatting function
    (filter(x$table_styling$fmt_fun, .data$column == "p.value") %>% pull("fmt_fun") %>% pluck(1)) %>%
    gts_mapper("add_q(pvalue_fun=)")

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.")
  }

  # perform multiple comparisons -----------------------------------------------
  expr_p.adjust <-
    rlang::expr(stats::p.adjust(x$table_body$p.value, method = !!method)) %>%
    deparse()
  if (quiet == FALSE) {
    rlang::inform(glue("add_q: Adjusting p-values with\n`{expr_p.adjust}`"))
  }

  x$table_body$q.value <- x$table_body$p.value %>% stats::p.adjust(method = method)

  # update table_styling -------------------------------------------------------
  # footnote text
  footnote_text <-
    add_q_method_lookup[add_q_method_lookup$method == method, ]$method_label %>%
    translate_text()
  x <-
    modify_table_styling(
      x,
      columns = "q.value",
      footnote = footnote_text,
      fmt_fun = pvalue_fun
    )

  # adding  column header
  x <- modify_header(x, q.value = paste0("**", translate_text("q-value"), "**"))

  # return final object --------------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  # adding call
  x$call_list <- updated_call_list

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
