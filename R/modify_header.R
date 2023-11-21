

modify_header <- function(x, ..., text_interpret = c("md", "html"),
                          quiet = NULL, update = NULL, stat_by = NULL) {
  # process inputs -------------------------------------------------------------
  dots <- rlang::dots_list(...)
  text_interpret <- rlang::arg_match(text_interpret)

  # deprecated arguments
  if (!is.null(stat_by)) {
    lifecycle::deprecate_stop(
      "1.3.6", "gtsummary::modify_header(stat_by=)",
      details = glue("Use `all_stat_cols(FALSE) ~ {stat_by}` instead.")
    )
  }
  if (!is.null(update)) {
    lifecycle::deprecate_warn(
      "2.0.0", "gtsummary::modify_header(update=)",
      details = "Use `modify_header(...)` input instead."
    )
    if (is.factor(dots)) dots <- c(list(dots), update)
    else dots <- c(list(dots), update)
  }

  cards::process_formula_selectors(data = x$table_body, dots = dots)

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
