#' Modify column headers, footnotes, and spanning headers
#'
#' These functions assist with updating or adding column headers
#' (`modify_header()`), footnotes (`modify_footnote()`), and spanning
#' headers (`modify_spanning_header()`). Use `show_header_names()` to learn
#' the column names.
#'
#' @name modify
#' @param x a gtsummary object
#' @param update list of formulas or a single formula specifying the updated
#' column header, footnote, or spanning header.
#' The LHS specifies the column(s) to be updated, and the RHS is the updated text.
#' Use the `show_header_names()` to see the column names that can be modified.
#' @param abbreviation Logical indicating if an abbreviation is being updated.
#' @param stat_by Used with `tbl_summary(by=)` objects with a `by=` argument.
#' String specifying text to include above the summary statistics.
#' The following fields are available for use in the
#' headers:
#' * `{n}` number of observations in each group,
#' * `{N}` total number of observations,
#' * `{p}` percentage in each group,
#' * `{level}` the 'by' variable level,
#'
#' Syntax follows [glue::glue()],
#' e.g. `stat_by = "**{level}**, N = {n} ({style_percent(p)}%)"`.
#' @param ... Specify a column and updated column label,
#' e.g. `modify_header(p.value = "Model P-values")`. This is provided as an alternative to the
#' `update=` argument. They accomplish the same goal of updating column headers.
#' @param text_interpret String indicates whether text will be interpreted with
#' [gt::md()] or [gt::html()]. Must be `"md"` (default) or `"html"`.
#' @inheritParams add_global_p.tbl_regression
#' @family tbl_summary tools
#' @family tbl_svysummary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_survfit tools
#' @author Daniel D. Sjoberg
#' @examples
#' # create summary table
#' tbl <- trial[c("age", "grade", "trt")] %>%
#'   tbl_summary(by = trt, missing = "no") %>%
#'   add_p()
#'
#' # print the column names that can be modified
#' show_header_names(tbl)
#'
#' # Example 1 ----------------------------------
#' # updating column headers and footnote
#' modify_ex1 <- tbl %>%
#'   modify_header(
#'     update = list(label ~ "**Variable**",
#'                   p.value ~ "**P**")
#'   ) %>%
#'   modify_footnote(
#'     update = starts_with("stat_") ~ "median (IQR) for Age; n (%) for Grade"
#'   )
#'
#' # Example 2 ----------------------------------
#' # using `stat_by=` argument to update headers, remove all footnotes, add spanning header
#' modify_ex2 <- tbl %>%
#'   modify_header(stat_by = "**{level}**, N = {n} ({style_percent(p)}%)") %>%
#'   # use `modify_footnote(everything() ~ NA, abbreviation = TRUE)` to delete abbrev. footnotes
#'   modify_footnote(update = everything() ~ NA) %>%
#'   modify_spanning_header(starts_with("stat_") ~ "**Treatment Received**")
#'
#' # Example 3 ----------------------------------
#' # updating an abbreviation in table footnote
#' modify_ex3 <-
#'   glm(response ~ age + grade, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   modify_footnote(ci ~ "CI = Credible Interval", abbreviation = TRUE)
#'
#' @return Updated gtsummary object
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_ex1.png}{options: width=45\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{modify_ex2.png}{options: width=45\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{modify_ex3.png}{options: width=35\%}}
NULL

#' @name modify
#' @export
modify_header <- function(x, update = NULL, stat_by = NULL,
                          text_interpret = c("md", "html"), ...) {
  # update table_header --------------------------------------------------------
  x$table_header <- table_header_fill_missing(x$table_header, x$table_body)

  # converting update arg to a tidyselect list ---------------------------------
  update <-
    tidyselect_to_list(x$table_body, {{ update }}, arg_name = "update") %>%
    # adding the ... to the update list
    c(list(...))

  # running modify_header_internal function ------------------------------------
  rlang::call2(
    modify_header_internal,
    x = x,
    stat_by = stat_by,
    text_interpret = text_interpret,
    !!!update,
    .save_call = TRUE
  ) %>%
    eval()
}

#' @name modify
#' @export
modify_footnote <- function(x, update, abbreviation = FALSE) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Argument `x=` must be an object with 'gtsummary' class", call. = FALSE)
  }

  # update table_header --------------------------------------------------------
  x$table_header <- table_header_fill_missing(x$table_header, x$table_body)

  # converting update arg to a tidyselect list ---------------------------------
  update <- tidyselect_to_list(x$table_body, {{ update }}, arg_name = "update")

  # updating footnote ----------------------------------------------------------
  footnote_column_name <- ifelse(abbreviation == TRUE, "footnote_abbrev", "footnote")

  # convert named list to a tibble
  table_header_update <-
    update %>%
    unlist() %>%
    tibble::enframe(name = "column", value = footnote_column_name) %>%
    # ensuring the column is a character
    mutate_at(vars(any_of(footnote_column_name)), as.character) %>%
    # performing inner join to put the edits in the same order as x$table_header
    {dplyr::inner_join(
      x$table_header %>% select(.data$column),
      .,
      by = "column"
    )}

  # updating table_header
  rows <- x$table_header$column %in% table_header_update$column
  x$table_header[rows, c("column", footnote_column_name)] <-
    table_header_update

  # return updated gtsummary object --------------------------------------------
  x[["call_list"]] <- list(x[["call_list"]], modify_footnote = match.call())
  x
}

#' @name modify
#' @export
modify_spanning_header <- function(x, update) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Argument `x=` must be an object with 'gtsummary' class", call. = FALSE)
  }

  # update table_header --------------------------------------------------------
  x$table_header <- table_header_fill_missing(x$table_header, x$table_body)

  # converting update arg to a tidyselect list ---------------------------------
  update <- tidyselect_to_list(x$table_body, {{ update }}, arg_name = "update")

  # updating footnote ----------------------------------------------------------
  # convert named list to a tibble
  table_header_update <-
    update %>%
    unlist() %>%
    tibble::enframe(name = "column", value = "spanning_header") %>%
    # ensuring the column is a character
    mutate_at(vars(.data$spanning_header), as.character) %>%
    # performing inner join to put the edits in the same order as x$table_header
    {dplyr::inner_join(
      x$table_header %>% select(.data$column),
      .,
      by = "column"
    )}

  # updating table_header
  rows <- x$table_header$column %in% table_header_update$column
  x$table_header[rows, c("column", "spanning_header")] <-
    table_header_update

  # return updated gtsummary object --------------------------------------------
  x[["call_list"]] <- list(x[["call_list"]], add_p = match.call())
  x
}

#' @name modify
#' @export
show_header_names <- function(x = NULL, quiet = NULL) {
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking input -------------------------------------------------------------
  if (!inherits(x, "gtsummary"))
    stop("Pass a 'gtsummary' object in `x=` to print current column names and headers.")

  df_cols <- x$table_header %>%
    filter(.data$hide == FALSE) %>%
    select(.data$column, .data$label)

  if (identical(quiet, FALSE)) {
    knitr::kable(df_cols, col.names = c("Column Name", "Column Header"), format = "pandoc") %>%
      print()

    cat("\n")
    usethis::ui_info("As a usage guide, the code below re-creates the current column headers.")
    block <- mutate(df_cols, formula = glue("  {column} ~ {shQuote(label)}")) %>%
      pull(.data$formula) %>%
      paste0("", collapse = ",\n") %>%
      {glue("modify_header(update = list(\n{.}\n))")}

    ui_code_block(block)
  }

  return(invisible(df_cols))
}
