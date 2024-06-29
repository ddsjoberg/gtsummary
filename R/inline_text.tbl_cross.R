#' Report statistics from cross table inline
#'
#' \lifecycle{maturing}
#' Extracts and returns statistics from a `tbl_cross` object for
#' inline reporting in an R markdown document. Detailed examples in the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html}{inline_text vignette}
#' @param x (`tbl_cross`)\cr
#'   A `tbl_cross` object
#' @param row_level (`string`)\cr
#'   Level of the row variable to display.
#' @param col_level (`string`)\cr
#'   Level of the column variable to display.
#'   Can also specify "`p.value`" for the p-value and "`stat_0`" for Total column.
#' @inheritParams inline_text.tbl_summary
#'
#' @return A string reporting results from a gtsummary table
#'
#' @export
#' @examples
#' tbl_cross <-
#'   tbl_cross(trial, row = trt, col = response) %>%
#'   add_p()
#'
#' inline_text(tbl_cross, row_level = "Drug A", col_level = "1")
#' inline_text(tbl_cross, row_level = "Total", col_level = "1")
#' inline_text(tbl_cross, col_level = "p.value")
inline_text.tbl_cross <- function(x,
                                  col_level,
                                  row_level = NULL,
                                  pvalue_fun = label_style_pvalue(prepend_p = TRUE),
                                  ...) {
  set_cli_abort_call()
  check_dots_empty()
  check_not_missing(col_level)

  # setting defaults -----------------------------------------------------------
  pvalue_fun <-
    case_switch(
      missing(pvalue_fun) ~ get_theme_element("pkgwide-fn:prependpvalue_fun", default = pvalue_fun),
      .default = pvalue_fun
    )
  pvalue_fun <- as_function(pvalue_fun)

  x <- modify_fmt_fun(x, any_of(c("p.value", "q.value")) ~ pvalue_fun)

  # row_level ----------------------------------------------------------------
  # converting row_level to a string
  row_level <-
    .select_levels(lvl = {{ row_level }}, possible_lvls = x$table_body$label, lvl_argname =  "row_level", allow_empty = TRUE)
  if (is_empty(row_level)) variable <- x$inputs$row
  else {
    variable <-
      deframe(x$table_body[c("variable", "label")])[x$table_body$label %in% row_level] |>
      names()
  }
  if (identical(variable, "..total..")) row_level <- NULL # styler: off

  # col_level ------------------------------------------------------------------
  col_var_lvls <- x$cards[[1]] |>
    dplyr::filter(.data$context == "attributes",
                  .data$variable == x$inputs$col,
                  .data$stat_name == "levels") |>
    dplyr::pull("stat") |>
    unlist()
  col_level <-
    .select_levels(
      lvl = {{ col_level }},
      possible_lvls =
        col_var_lvls |>
        c(intersect(c("stat_0", "p.value"), names(x$table_body))),
      lvl_argname =  "col_level",
      allow_empty = FALSE
    )
  if (col_level %in% col_var_lvls) {
    column <- paste0("stat_", which(col_var_lvls %in% col_level))
  } else {
    column <- col_level
  }

  # evaluating inline_text for tbl_summary -----------------------------------
  inject(
    inline_text.tbl_summary(
      x,
      variable = !!variable,
      level = !!row_level,
      column = !!column,
      pvalue_fun = pvalue_fun
    )
  )
}

