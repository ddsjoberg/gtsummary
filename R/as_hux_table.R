#' Convert gtsummary object to a huxtable object
#'
#' Function converts a gtsummary object to a huxtable object.
#' A user can use this function if they wish to add customized formatting
#' available via the huxtable functions. The huxtable package supports output
#' to PDF via LaTeX, as well as HTML and Word.
#'
#' @section Excel Output:
#'
#' Use the `as_hux_xlsx()` function to save a copy of the table in an excel file.
#' The file is saved using `huxtable::quick_xlsx()`.
#'
#' @inheritParams as_flex_table
#' @inheritParams huxtable::quick_xlsx
#' @param bold_header_rows (scalar `logical`)\cr
#'   logical indicating whether to bold header rows. Default is `TRUE`
#' @param strip_md_bold `r lifecycle::badge("deprecated")`
#'
#' @name as_hux_table
#' @return A \{huxtable\} object
#'
#' @author David Hugh-Jones, Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("huxtable", reference_pkg = "gtsummary")
#' trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   add_p() |>
#'   as_hux_table()
NULL

#' @export
#' @rdname as_hux_table
as_hux_table <- function(x, include = everything(), return_calls = FALSE,
                         strip_md_bold = FALSE) {
  set_cli_abort_call()
  check_class(x, "gtsummary")
  check_pkg_installed("huxtable", reference_pkg = "gtsummary")
  check_scalar_logical(return_calls)

  if (!isFALSE(strip_md_bold)) {
    lifecycle::deprecate_stop(
      "1.6.0", "gtsummary::as_hux_table(strip_md_bold=)",
      details = "Markdown syntax is now recognized by the {huxtable} package."
    )
  }
  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .table_styling_expr_to_row_number(x)

  # creating list of huxtable calls -------------------------------------------
  huxtable_calls <- table_styling_to_huxtable_calls(x = x)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_hux_table.gtsummary-lst:addl_cmds")
  huxtable_calls <-
    reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) {
        add_expr_after(
          calls = x,
          add_after = names(insert_expr_after[y]),
          expr = insert_expr_after[[y]],
          new_name = paste0("user_added", y)
        )
      },
      .init = huxtable_calls
    )

  # converting to character vector ----------------------------------------------
  cards::process_selectors(
    data = vec_to_df(names(huxtable_calls)),
    include = {{ include }}
  )

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) {
    return(huxtable_calls[include])
  }

  .eval_list_of_exprs(huxtable_calls[include])
}

#' @export
#' @rdname as_hux_table
as_hux_xlsx <- function(x, file, include = everything(), bold_header_rows = TRUE) {
  set_cli_abort_call()
  check_class(x, "gtsummary")
  check_pkg_installed(c("huxtable", "openxlsx"), reference_pkg = "gtsummary")
  check_scalar_logical(bold_header_rows)


}
