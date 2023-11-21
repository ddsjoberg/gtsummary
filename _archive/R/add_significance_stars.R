#' Add significance stars
#'
#' \lifecycle{experimental}
#' Add significance stars to estimates with small p-values
#'
#' @param x a `'gtsummary'` object with a `'p.value'` column
#' @param thresholds thresholds for significance stars. Default is `c(0.001, 0.01, 0.05)`
#' @param hide_ci logical whether to hide confidence interval. Default is `TRUE`
#' @param hide_p logical whether to hide p-value. Default is `TRUE` for regression summaries, and `FALSE` otherwise.
#' @param hide_se logical whether to hide standard error. Default is `FALSE`
#' @param pattern glue-syntax string indicating what to display in formatted column.
#' Default is `"{estimate}{stars}"` for regression summaries and `"{p.value}{stars}"` otherwise.
#' A footnote is placed on the first column listed in the pattern.
#' Other common patterns are
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
#' @examplesIf broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE)
#' \donttest{
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
#' # Use '  \n' to put a line break between beta and SE
#' add_significance_stars_ex3 <-
#'   tbl %>%
#'   add_significance_stars(
#'     hide_se = TRUE,
#'     pattern = "{estimate}{stars}  \n({std.error})"
#'   ) %>%
#'   modify_header(estimate ~ "**Beta  \n(SE)**") %>%
#'   modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
#'   as_gt() %>%
#'   gt::fmt_markdown(columns = everything()) %>%
#'   gt::tab_style(
#'     style = "vertical-align:top",
#'     locations = gt::cells_body(columns = label)
#'   )
#'
#' # Example 4 ----------------------------------
#' add_significance_stars_ex4 <-
#'   lm(marker ~ stage + grade, data = trial) %>%
#'   tbl_regression() %>%
#'   add_global_p() %>%
#'   add_significance_stars(
#'     hide_p = FALSE,
#'     pattern = "{p.value}{stars}"
#'   ) %>%
#'   as_gt() %>%
#'   gt::tab_style(
#'     style = "vertical-align:top",
#'     locations = gt::cells_body(columns = label)
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_significance_stars_ex1.png", width = "45")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_significance_stars_ex2.png", width = "35")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_significance_stars_ex3.png", width = "30")`
#' }}
#'
#' \if{html}{Example 4}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "add_significance_stars_ex4.png", width = "30")`
#' }}

add_significance_stars <- function(x, pattern = NULL,
                                   thresholds = c(0.001, 0.01, 0.05),
                                   hide_ci = TRUE,
                                   hide_p = inherits(x, c("tbl_regression", "tbl_uvregression")),
                                   hide_se = FALSE) {
  # checking inputs ------------------------------------------------------------
  .assert_class(x, "gtsummary")
  if (!"p.value" %in% names(x$table_body)) {
    cli::cli_abort(c(
      "!" = "There is no p-value column in the table and significance stars cannot be placed."
    ))
  }

  # assign default pattern and footnote placement ------------------------------
  pattern <-
    pattern %||%
    dplyr::case_when(
      inherits(x, c("tbl_regression", "tbl_uvregression")) ~ "{estimate}{stars}",
      TRUE ~ "{p.value}{stars}"
    )

  thresholds <- sort(thresholds, decreasing = TRUE) %>% unique()
  if (any(!dplyr::between(thresholds, 0L, 1L))) {
    abort("All thresholds must be between 0 and 1.")
  }

  if (!is_string(pattern)) abort("`pattern=` must be a string.")
  pattern_cols <-
    str_extract_all(pattern, "\\{.*?\\}") %>%
    map(~ str_remove_all(.x, pattern = fixed("}"))) %>%
    map(~ str_remove_all(.x, pattern = fixed("{"))) %>%
    unlist()
  if (isTRUE(is_empty(pattern_cols))) {
    abort("`pattern=` must be a string using glue syntax to select columns.")
  }
  if (!"stars" %in% pattern_cols) {
    inform("`pattern=` argument does not contain '{stars}' column, and no stars will be added.")
  }
  updated_call_list <- c(x$call_list, list(add_significance_stars = match.call()))

  # adding footnote ------------------------------------------------------------
  p_footnote <-
    paste0(
      purrr::imap_chr(thresholds, ~ rep_len("*", .y) %>% paste(collapse = "")),
      "p<",
      thresholds
    ) %>%
    paste(collapse = "; ")

  x <- modify_footnote(x, any_of(pattern_cols[1]) ~ p_footnote)

  # adding stars column --------------------------------------------------------
  thresholds <- union(thresholds, 0L)
  expr_stars_case_when <-
    map2(
      thresholds, seq_along(thresholds),
      ~ expr(p.value >= !!.x ~ !!paste(rep_len("*", .y - 1), collapse = "")) %>%
        rlang::expr_deparse()
    ) %>%
    purrr::reduce(.f = ~ paste(.x, .y, sep = ", ")) %>%
    {
      paste0("dplyr::case_when(is.na(p.value) ~ '', ", ., ")")
    } %>%
    rlang::parse_expr()

  x <- modify_table_body(x, ~ .x %>% dplyr::mutate(stars = !!expr_stars_case_when))

  # updating hidden column status ----------------------------------------------
  cols_to_hide <- c(ci = hide_ci, p.value = hide_p, std.error = hide_se)
  cols_to_hide <- cols_to_hide[c("ci", "p.value", "std.error") %in% names(x$table_body)]
  x <-
    x %>%
    modify_table_styling(
      columns = all_of(names(cols_to_hide)),
      hide = cols_to_hide
    )

  # adding `cols_merge` to table styling ---------------------------------------
  x <-
    modify_table_styling(
      x = x,
      columns = pattern_cols[1],
      rows =
        !!expr(!is.na(.data$p.value)),
      cols_merge_pattern = pattern
    )

  # return x -------------------------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- .fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}
