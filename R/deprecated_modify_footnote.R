#' DEPRECATED Footnote
#'
#' `r lifecycle::badge("deprecated")`\cr
#' Use [`modify_footnote_header()`] and [`modify_abbreviation()`] instead.
#'
#' @inheritParams modify
#' @param abbreviation (scalar `logical`)\cr
#'   Logical indicating if an abbreviation is being updated.
#' @param ... [`dynamic-dots`][rlang::dyn-dots]\cr
#'   Used to assign updates to footnotes.
#'   Use `modify_footnote(colname='new footnote')` to update a single footnote.
#' @return Updated gtsummary object
#' @export
#' @keywords internal
#' @name deprecated_modify_footnote
#'
#' @examples
#' # Use `modify_footnote_header()`, `modify_footnote_body()`, `modify_abbreviation()` instead.
modify_footnote <- function(x, ..., abbreviation = FALSE,
                            text_interpret = c("md", "html"),
                            update, quiet) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_footnote = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  text_interpret <- arg_match(text_interpret)

  # process inputs -------------------------------------------------------------
  dots <- rlang::dots_list(...)
  dots <- .deprecate_modify_update_and_quiet_args(dots, update, quiet, calling_fun = "modify_footnote")

  # process arguments ----------------------------------------------------------
  text_interpret <- rlang::arg_match(text_interpret)
  cards::process_formula_selectors(data = scope_header(x$table_body, x$table_styling$header), dots = dots)
  cards::check_list_elements(
    x = dots,
    predicate = function(x) is_string(x) || is.na(x),
    error_msg =
      c("All values passed in {.arg ...} must be strings.",
        "i" = "For example, {.code label='Results as of June 26, 2015'}"
      )
  )

  # evaluate the strings with glue
  dots <- .evaluate_string_with_glue(x, dots)

  # updating footnotes ---------------------------------------------------------
  x <-
    if (!abbreviation) {
      modify_table_styling(
        x = x,
        columns = names(dots),
        footnote = unlist(dots),
        text_interpret = text_interpret
      )
    } else {
      modify_table_styling(
        x = x,
        columns = names(dots),
        footnote_abbrev = unlist(dots),
        text_interpret = text_interpret
      )
    }

  # returning gtsummary object -------------------------------------------------
  x$call_list <- updated_call_list
  x
}
