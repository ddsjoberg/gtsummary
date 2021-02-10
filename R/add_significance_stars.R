#' Add significance stars
#'
#' \lifecycle{experimental}
#' Add significance stars to estimates with small p-values
#'
#' @param x a `'tbl_regression'` or `'tbl_uvregression'` object
#' @param thresholds thresholds for significance stars. Default is `c(0.001, 0.01, 0.05)`
#' @param hide_ci logical whether to hide confidence interval. Default is `TRUE`
#' @param hide_p logical whether to hide p-value. Default is `TRUE`
#' @param hide_se logical whether to hide standard error. Default is `FALSE`
#'
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' add_significance_stars_ex1 <-
#'   lm(time ~ sex + ph.ecog, survival::lung) %>%
#'   tbl_regression() %>%
#'   add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_significance_stars_ex1.png}{options: width=45\%}}

add_significance_stars <- function(x, thresholds = c(0.001, 0.01, 0.05),
                                   hide_ci = TRUE, hide_p = TRUE, hide_se = FALSE) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression") && !inherits(x, "tbl_uvregression"))
    abort("x=` must be a 'tbl_regression' or 'tbl_uvregression' object.")
  thresholds <- sort(thresholds, decreasing = TRUE) %>% unique()
  if (any(!dplyr::between(thresholds, 0L, 1L)))
    abort("All thresholds must be between 0 and 1.")

  # assigning estimate fun -----------------------------------------------------
  estimate_fun <-
    x$inputs$estimate_fun %||%
    get_theme_element("tbl_regression-arg:estimate_fun") %||%
    getOption(
      "gtsummary.tbl_regression.estimate_fun",
      default = ifelse(x$inputs$exponentiate == TRUE, style_ratio, style_sigfig)
    ) %>%
    gts_mapper("add_significance_stars(estimate_fun=)")

  # adding footnote ------------------------------------------------------------
  p_footnote <-
    paste0(
      purrr::imap_chr(thresholds, ~rep_len("*", .y) %>% paste(collapse = "")),
      "p<",
      thresholds
    ) %>%
    paste(collapse = "; ")

  x <- modify_footnote(x, "estimate" ~ p_footnote)

  # formatting the estimate with significance stars ----------------------------
  # adding 0 onto the thresholds list
  thresholds <- union(thresholds, 0L)

  for (i in seq_len(length(thresholds) - 1)) {
    x <-
      x %>%
      modify_table_styling(
        columns = "estimate",
        rows = !!expr(.data$p.value < !!thresholds[i] & .data$p.value >= !!thresholds[i + 1]),
        fmt_fun = expr(function(x) estimate_fun(x) %>% paste0(paste(rep_len("*", !!i), collapse = ""))) %>% eval()
      )
  }

  # updating hidden column status ----------------------------------------------
  x <-
    x %>%
    modify_table_styling(
      columns = c("ci", "p.value", "std.error"),
      hide = c(hide_ci, hide_p, hide_se)
    )

  # return x -------------------------------------------------------------------
  x$call_list <- c(x$call_list, list(add_significance_stars = match.call()))
  x
}
