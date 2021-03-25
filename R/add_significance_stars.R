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
#' @param pattern glue-syntax string indicating what to display in formatted column.
#' Default is `"{estimate}{stars}"`. Other common patterns are
#' `"{estimate}{stars} ({conf.low}, {conf.high})"` and
#' `"{estimate} ({conf.low} to {conf.high}){stars}"`
#'
#' @export
#' @section Future Updates:
#' There are planned updates to the implementation of this function
#' with respect to the `pattern=` argument.
#' Currently, this function replaces the numeric estimate column, with a
#' formatted character column following `pattern=`.
#' Once `gt::cols_merge()` gains the `rows=` argument the
#' implementation will be updated to use it, which will keep
#' numeric columns numeric. For the _vast majority_ of users,
#' _the planned change will be go unnoticed_.
#'
#' @examples
#' tbl <-
#'   lm(time ~ ph.ecog + sex, survival::lung) %>%
#'   tbl_regression(label = list(ph.ecog = "ECOG Score", sex = "Sex"))
#'
#' # Example 1 ----------------------------------
#' add_significance_stars_ex1 <-
#'   tbl %>%
#'   add_significance_stars(hide_ci = FALSE, hide_p = FALSE)
#'
#' # Example 2 ----------------------------------
#' add_significance_stars_ex2 <-
#'   tbl %>%
#'   add_significance_stars(
#'     pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
#'     hide_ci = TRUE, hide_se = TRUE
#'   ) %>%
#'   modify_header(estimate ~ "**Beta (95% CI)**") %>%
#'   modify_footnote(estimate ~ "CI = Confidence Interval", abbreviation = TRUE)
#'
#' # Example 3 ----------------------------------
#' # Use <br> to put a line break between beta and SE in HTML output
#' add_significance_stars_ex3 <-
#'   tbl %>%
#'   add_significance_stars(
#'     hide_se = TRUE,
#'     pattern = "{estimate}{stars}<br>({std.error})"
#'   ) %>%
#'   modify_header(estimate ~ "**Beta (SE)**") %>%
#'   modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_significance_stars_ex1.png}{options: width=45\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_significance_stars_ex2.png}{options: width=35\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{add_significance_stars_ex3.png}{options: width=30\%}}

add_significance_stars <- function(x, pattern = "{estimate}{stars}",
                                   thresholds = c(0.001, 0.01, 0.05),
                                   hide_ci = TRUE, hide_p = TRUE, hide_se = FALSE) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "tbl_regression") && !inherits(x, "tbl_uvregression"))
    abort("x=` must be a 'tbl_regression' or 'tbl_uvregression' object.")
  thresholds <- sort(thresholds, decreasing = TRUE) %>% unique()
  if (any(!dplyr::between(thresholds, 0L, 1L)))
    abort("All thresholds must be between 0 and 1.")

  if (!is_string(pattern)) abort("`pattern=` must be a string.")
  pattern_cols <-
    str_extract_all(pattern, "\\{.*?\\}") %>%
    map(str_remove_all, pattern = fixed("}")) %>%
    map(str_remove_all, pattern = fixed("{")) %>%
    unlist()
  if (isTRUE(is_empty(pattern_cols)))
    abort("`pattern=` must be a string using glue syntax to select columns.")

  # adding footnote ------------------------------------------------------------
  p_footnote <-
    paste0(
      purrr::imap_chr(thresholds, ~rep_len("*", .y) %>% paste(collapse = "")),
      "p<",
      thresholds
    ) %>%
    paste(collapse = "; ")

  x <- modify_footnote(x, "estimate" ~ p_footnote)

  # adding stars column --------------------------------------------------------
  thresholds <- union(thresholds, 0L)
  expr_stars_case_when <-
    map2(
      thresholds, seq_along(thresholds),
      ~expr(!is.na(estimate) & p.value >= !!.x ~ !!paste(rep_len("*", .y - 1), collapse = "")) %>%
        rlang::expr_deparse()
    ) %>%
    purrr::reduce(.f = ~paste(.x, .y, sep = ", ")) %>%
    {paste0("dplyr::case_when(", ., ")")} %>%
    rlang::parse_expr()

  x <- modify_table_body(x, ~.x %>% dplyr::mutate(stars = !!expr_stars_case_when))

  # updating hidden column status ----------------------------------------------
  x <-
    x %>%
    modify_table_styling(
      columns = c("ci", "p.value", "std.error"),
      hide = c(hide_ci, hide_p, hide_se)
    )

  # adding `cols_merge` to table styling ---------------------------------------
  model_variables <-
    x$table_body %>%
    filter(!is.na(.data$coefficients_type)) %>% # keep obs from regression model
    dplyr::pull(.data$variable) %>%
    unique()

  x$table_styling$cols_merge <-
    tibble(
      column = "estimate",
      rows = expr(.data$variable %in% !!model_variables & !is.na(.data$estimate)) %>% list(),
      pattern = pattern
    )

  # return x -------------------------------------------------------------------
  x$call_list <- c(x$call_list, list(add_significance_stars = match.call()))
  x
}
