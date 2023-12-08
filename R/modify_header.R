

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
    if (is.factor(dots)) dots <- c(list(dots), update)
    else dots <- c(list(dots), update)
  }

  cards::process_formula_selectors(data = x$table_body, dots = dots)
  cards::check_list_elements(
    dots = function(x) is_string(x),
    error_msg = list(dots = c("All values passed in {.arg ...} must be strings.",
                              "i" = "For example, {.code label = '**Variable**'}"))
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

      if (!is.null(glued_value$result))
        return(glued_value$result)

      cli::cli_abort("There was an error the {.fun glue::glue} evaluation of {.val {value}}.")
    }
  )
}
