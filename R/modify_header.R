#' Modify column headers, footnotes, spanning headers, and table captions
#'
#' These functions assist with updating or adding column headers
#' (`modify_header()`), footnotes (`modify_footnote()`), spanning
#' headers (`modify_spanning_header()`), and table captions
#' (`modify_caption()`). Use `show_header_names()` to learn
#' the column names.
#'
#' @name modify
#' @param x a gtsummary object
#' @param update,... use these arguments to assign updates to headers,
#' spanning headers, and footnotes. See examples below.
#' - `update` expects a list of assignments, with the variable name or selector
#' on the LHS of the formula, and the updated string on the RHS. Also accepts
#' a named list.
#' - `...` pass individual updates outside of a list, e.g,
#' `modify_header(p.value = "**P**", all_stat_cols() ~ "**{level}**")`
#'
#' Use the `show_header_names()` to see the column names that can be modified.
# #' @param abbreviation Logical indicating if an abbreviation is being updated.
#' @param text_interpret String indicates whether text will be interpreted with
#' [gt::md()] or [gt::html()]. Must be `"md"` (default) or `"html"`.
# #' @param caption a string of the table caption/title
# #' @param include_example logical whether to include print of `modify_header()` example
#' @param quiet (scalar `logical`)\cr
#'   When `TRUE`, additional messaging is not printed.
#'   TODO: Set defaults here and theme control of argument
# #' @inheritParams modify_table_styling
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_survfit tools
#' @author Daniel D. Sjoberg
#'
#' @section tbl_summary(), tbl_svysummary(), and tbl_cross():
#' When assigning column headers, footnotes, spanning headers, and captions
#' for these gtsummary tables,
#' you may use `{N}` to insert the number of observations.
#' `tbl_svysummary` objects additionally have `{N_unweighted}` available.
#'
#' When there is a stratifying `by=` argument present, the following fields are
#' additionally available to stratifying columns: `{level}`, `{n}`, and `{p}`
#' (`{n_unweighted}` and `{p_unweighted}` for `tbl_svysummary` objects)
#'
#' Syntax follows [glue::glue()], e.g. `all_stat_cols() ~ "**{level}**, N = {n}"`.
#' @section tbl_regression():
#' When assigning column headers for `tbl_regression` tables,
#' you may use `{N}` to insert the number of observations, and `{N_event}`
#' for the number of events (when applicable).
#'
#' @section captions:
#' Captions are assigned based on output type.
#' - `gt::gt(caption=)`
#' - `flextable::set_caption(caption=)`
#' - `huxtable::set_caption(value=)`
#' - `knitr::kable(caption=)`
#'
#' @examplesIf FALSE
#' \donttest{
#' # create summary table
#' tbl <- trial[c("age", "grade", "trt")] %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_p()
#'
#' # print the column names that can be modified
#' show_header_names(tbl)
#'
#' # Example 1 ----------------------------------
#' # updating column headers, footnote, and table caption
#' modify_ex1 <- tbl %>%
#'   modify_header(label = "**Variable**", p.value = "**P**") %>%
#'   modify_footnote(all_stat_cols() ~ "median (IQR) for Age; n (%) for Grade") %>%
#'   modify_caption("**Patient Characteristics** (N = {N})")
#'
#' # Example 2 ----------------------------------
#' # updating headers, remove all footnotes, add spanning header
#' modify_ex2 <- tbl %>%
#'   modify_header(all_stat_cols() ~ "**{level}**, N = {n} ({style_percent(p)}%)") %>%
#'   # use `modify_footnote(everything() ~ NA, abbreviation = TRUE)` to delete abbrev. footnotes
#'   modify_footnote(update = everything() ~ NA) %>%
#'   modify_spanning_header(all_stat_cols() ~ "**Treatment Received**")
#'
#' # Example 3 ----------------------------------
#' # updating an abbreviation in table footnote
#' modify_ex3 <-
#'   glm(response ~ age + grade, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   modify_footnote(ci = "CI = Credible Interval", abbreviation = TRUE)
#' }
#' @return Updated gtsummary object
NULL

#' @name modify
#' @export
modify_header <- function(x, ..., text_interpret = c("md", "html"),
                          quiet = NULL, update = NULL) {
  # process inputs -------------------------------------------------------------
  dots <- rlang::dots_list(...)
  text_interpret <- rlang::arg_match(text_interpret)

  # deprecated arguments
  if (!is.null(update)) {
    lifecycle::deprecate_warn(
      "2.0.0", "gtsummary::modify_header(update=)",
      details = "Use `modify_header(...)` input instead."
    )
    if (is.factor(dots)) {
      dots <- c(list(dots), update)
    } else {
      dots <- c(list(dots), update)
    }
  }

  cards::process_formula_selectors(data = x$table_body, dots = dots)
  cards::check_list_elements(
    x = dots,
    predicate = function(x) is_string(x),
    error_msg =
      c("All values passed in {.arg ...} must be strings.",
        "i" = "For example, {.code label = '**Variable**'}"
      )
  )

  # evaluate the strings with glue
  dots <- .evaluate_string_with_glue(x, dots)

  # updated header meta data
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::mutate(
      hide = ifelse(.data$column %in% names(dots), FALSE, .data$hide)
    ) |>
    dplyr::rows_update(
      tibble::enframe(unlist(dots), name = "column", value = "label"),
      by = "column"
    )

  # return object
  x
}

.evaluate_string_with_glue <- function(x, dots) {
  # only keep values that are in the table_body
  dots <- dots[intersect(names(dots), x$table_styling$header$column)]

  df_header_subset <-
    x$table_styling$header |>
    dplyr::select("column", starts_with("modify_stat_")) |>
    dplyr::rename_with(
      .fn = function(x) gsub("^modify_stat_", "", x),
      .cols = starts_with("modify_stat_")
    )

  imap(
    dots,
    function(value, variable) {
      df_header_subset <-
        df_header_subset |>
        dplyr::filter(.data$column %in% .env$variable) |>
        dplyr::select(-"column")

      glued_value <-
        cards::eval_capture_conditions(
          expr(glue::glue(value)),
          data = df_header_subset
        )

      if (!is.null(glued_value$result)) {
        return(glued_value$result)
      }

      cli::cli_abort("There was an error the {.fun glue::glue} evaluation of {.val {value}}.")
    }
  )
}
