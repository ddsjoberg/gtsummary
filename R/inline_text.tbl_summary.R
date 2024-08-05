#' Report statistics from summary tables inline
#'
#' Extracts and returns statistics from a `tbl_summary()` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#'
#' @inheritParams inline_text.gtsummary
#' @inheritParams add_p.tbl_summary
#' @param x (`tbl_summary`)\cr
#'   Object created from  `tbl_summary()` or `tbl_svysummary()`
#' @param column ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name to return from `x$table_body`.
#'   Can also pass the level of a by variable.
#' @inheritParams rlang::args_dots_empty
#'
#' @author Daniel D. Sjoberg
#' @name inline_text.tbl_summary
#' @export
#' @return A string reporting results from a gtsummary table
#'
#' @examples
#' t1 <- trial |>
#'   tbl_summary(by = trt, include = grade) |>
#'   add_p()
#'
#' inline_text(t1, variable = grade, level = "I", column = "Drug A", pattern = "{n}/{N} ({p}%)")
#' inline_text(t1, variable = grade, column = "p.value")
NULL

#' @name inline_text.tbl_summary
#' @export
inline_text.tbl_summary <- function(x,
                                    variable,
                                    column = NULL,
                                    level = NULL,
                                    pattern = NULL,
                                    pvalue_fun = label_style_pvalue(prepend_p = TRUE),
                                    ...) {
  set_cli_abort_call()
  check_dots_empty()

  # setting defaults ---------------------------------------------------------
  pvalue_fun <-
    case_switch(
      missing(pvalue_fun) ~ get_theme_element("pkgwide-fn:prependpvalue_fun", default = pvalue_fun),
      .default = pvalue_fun
    )
  pvalue_fun <- as_function(pvalue_fun)

  x <- modify_fmt_fun(x, any_of(c("p.value", "q.value")) ~ pvalue_fun)

  if (!is_empty(pattern) && any(c("p.value", "q.value") %in% .extract_glue_elements(pattern))) {
    lst_fmt_fn_updates <- rep_named(c("p.value", "q.value"), list(pvalue_fun))
    x$cards <- .update_fmt_fn(x$cards, lst_fmt_fn_updates)
  }

  # setting column argument ----------------------------------------------------
  column <- rlang::enquo(column)
  if (!is_quo_empty(column) && !is_empty(x$inputs$by)) {
    lst_by_levels <-
      x$cards[[1]] |>
      dplyr::filter(.data$group1 %in% .env$x$inputs$by) |>
      dplyr::select("gts_column", "group1_level") |>
      unique() |>
      dplyr::mutate(group1_level = unlist(.data$group1_level) |> as.character()) |>
      deframe() |>
      as.list()

    cards::process_selectors(
      data = vec_to_df(c(names(x$table_body), unlist(lst_by_levels))),
      column = !!column
    )
    if (column %in% unlist(lst_by_levels)) {
      column <- names(lst_by_levels)[unlist(lst_by_levels) %in% column]
    }
    column <- rlang::enquo(column)
  }

  # call generic inline_text() function ----------------------------------------
  inline_text.gtsummary(
    x = x,
    variable = {{ variable }},
    level = {{ level }},
    column = !!column,
    pattern = pattern
  )
}

#' @name inline_text.tbl_summary
#' @export
inline_text.tbl_svysummary <- inline_text.tbl_summary
