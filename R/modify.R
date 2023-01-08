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
#' @param abbreviation Logical indicating if an abbreviation is being updated.
#' @param stat_by DEPRECATED, use `update = all_stat_cols() ~ "<label>"` instead.
#' @param text_interpret String indicates whether text will be interpreted with
#' [gt::md()] or [gt::html()]. Must be `"md"` (default) or `"html"`.
#' @param caption a string of the table caption/title
#' @param include_example logical whether to include print of `modify_header()` example
#' @inheritParams modify_table_styling
#' @inheritParams add_global_p
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
#' @examples
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
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "modify_ex1.png", width = "45")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "modify_ex2.png", width = "45")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "modify_ex3.png", width = "35")`
#' }}
NULL

#' @name modify
#' @export
modify_header <- function(x, update = NULL, ..., text_interpret = c("md", "html"),
                          quiet = NULL, stat_by = NULL) {
  updated_call_list <- c(x$call_list, list(modify_header = match.call()))
  .assert_class(x, "gtsummary")
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE
  text_interpret <- match.arg(text_interpret)

  # converting update and dots args to a tidyselect list -----------------------
  if (!is.null(stat_by)) {
    lifecycle::deprecate_stop(
      "1.3.6", "gtsummary::modify_header(stat_by=)",
      details = glue("Use `all_stat_cols(FALSE) ~ {stat_by}` instead.")
    )
  }
  update <- .combine_update_and_dots(x, update, ...)


  # if no columns selected, print helpful message
  if (is.null(update) && identical(quiet, FALSE)) .modify_no_selected_vars(x)
  if (is.null(update)) {
    return(x)
  }

  # evaluating update with glue ------------------------------------------------
  update <- .eval_with_glue(x, update)

  # updating column headers ----------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = names(update),
      label = unlist(update),
      text_interpret = as.character(text_interpret),
      hide = FALSE
    )

  # returning gtsummary object -------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @name modify
#' @export
modify_footnote <- function(x, update = NULL, ..., abbreviation = FALSE,
                            text_interpret = c("md", "html"), quiet = NULL) {
  updated_call_list <- c(x$call_list, list(modify_footnote = match.call()))
  # checking inputs ------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # update table_styling -------------------------------------------------------
  x <- .update_table_styling(x)

  # converting update arg to a tidyselect list ---------------------------------
  update <- .combine_update_and_dots(
    x,
    {
      update
    },
    ...
  )

  # if no columns selected, print helpful message
  if (identical(quiet, FALSE) && rlang::is_empty(update)) .modify_no_selected_vars(x)
  if (is.null(update)) {
    return(x)
  }

  # evaluating update with glue ------------------------------------------------
  update <- .eval_with_glue(x, update)

  # updating footnotes ---------------------------------------------------------
  modify_table_styling_args <- list(
    x = x,
    columns = names(update),
    footnote = unlist(update),
    text_interpret = text_interpret
  )
  if (isTRUE(abbreviation)) { # for abbreviations, update list names
    modify_table_styling_args <-
      stats::setNames(
        modify_table_styling_args,
        c("x", "columns", "footnote_abbrev", "text_interpret")
      )
  }
  x <- do.call(modify_table_styling, modify_table_styling_args)

  # returning gtsummary object -------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @name modify
#' @export
modify_spanning_header <- function(x, update = NULL, ...,
                                   text_interpret = c("md", "html"), quiet = NULL) {
  updated_call_list <- c(x$call_list, list(modify_spanning_header = match.call()))
  # checking inputs ------------------------------------------------------------
  .assert_class(x, "gtsummary")

  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # update table_styling --------------------------------------------------------
  x <- .update_table_styling(x)

  # converting update arg to a tidyselect list ---------------------------------
  update <- .combine_update_and_dots(
    x,
    {
      update
    },
    ...
  )

  # if no columns selected, print helpful message
  if (identical(quiet, FALSE) && rlang::is_empty(update)) .modify_no_selected_vars(x)
  if (is.null(update)) {
    return(x)
  }

  # evaluating update with glue ------------------------------------------------
  update <- .eval_with_glue(x, update)

  # updating spanning header ---------------------------------------------------
  x <-
    modify_table_styling(
      x,
      columns = names(update),
      spanning_header = unlist(update),
      text_interpret = text_interpret
    )

  # return updated gtsummary object --------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @name modify
#' @export
modify_caption <- function(x, caption, text_interpret = c("md", "html")) {
  # checking inputs ------------------------------------------------------------
  .assert_class(x, "gtsummary")
  if (!rlang::is_string(caption)) abort("`caption=` must be a string.")
  text_interpret <- match.arg(text_interpret)
  updated_call_list <- c(x$call_list, list(modify_caption = match.call()))

  # evaluating update with glue ------------------------------------------------
  if ("label" %in% x$table_styling$header$column) {
    caption <- # eval on the label column stats
      .eval_with_glue(x, list(label = caption)) %>%
      unlist() %>%
      unname()
  }

  # adding caption to gtsummary object ----------------------------------------
  x$table_styling$caption <- caption
  attr(x$table_styling$caption, "text_interpret") <- text_interpret

  # returning updated object ---------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @name modify
#' @export
show_header_names <- function(x = NULL, include_example = TRUE, quiet = NULL) {
  # setting defaults -----------------------------------------------------------
  quiet <- quiet %||% get_theme_element("pkgwide-lgl:quiet") %||% FALSE

  # checking input -------------------------------------------------------------
  .assert_class(x, "gtsummary")

  df_cols <-
    x$table_styling$header %>%
    filter(.data$hide == FALSE) %>%
    select("column", "label")

  if (identical(quiet, FALSE) && isTRUE(include_example)) {
    cat("\n")
    cli_alert_info("As a usage guide, the code below re-creates the current column headers.")
    block <- mutate(df_cols, formula = glue("  {column} = {shQuote(label)}")) %>%
      pull("formula") %>%
      paste0("", collapse = ",\n") %>%
      {
        glue("modify_header(\n{.}\n)")
      }

    cli_code(block)
  }
  if (identical(quiet, FALSE)) {
    knitr::kable(df_cols, col.names = c("Column Name", "Column Header"), format = "pandoc") %>%
      print()
  }

  return(invisible(df_cols))
}

# prints a helpful message when no columns were selected in the modify functions
.modify_no_selected_vars <- function(x) {
  paste(
    "No columns were selected.",
    "Use {.code quiet = TRUE} to supress these messages."
  ) %>%
    stringr::str_wrap() %>%
    cli_alert_info()

  show_header_names(x)
}

.combine_update_and_dots <- function(x, update, ...) {
  dots <- rlang::dots_list(...)

  if (!is.null(update) && !rlang::is_list(update)) {
    update <- list(update)
  }

  .formula_list_to_named_list(
    x = c(update, dots),
    data = x$table_body,
    var_info =
      x$table_styling$header %>%
        select("column", "hide", starts_with("modify_selector_")) %>%
        dplyr::rename_with(
          .fn = ~ stringr::str_remove(., pattern = fixed("modify_selector_")),
          starts_with("modify_selector_")
        ),
    arg_name = "... or update",
    type_check = chuck(type_check, "is_string_or_na", "fn"),
    type_check_msg = chuck(type_check, "is_string_or_na", "msg")
  )
}

.eval_with_glue <- function(x, update) {
  df_header <-
    x$table_styling$header %>%
    select("column", starts_with("modify_stat_")) %>%
    dplyr::rename_with(~ stringr::str_replace(., fixed("modify_stat_"), fixed("")))

  imap(
    update,
    function(x, y) {
      if (!y %in% df_header$column) {
        cli::cli_alert_warning("Column {.val {y}} not found and was ignored.")
        return(NULL)
      }
      lst_env_for_eval <-
        df_header %>%
        filter(.data$column %in% .env$y) %>%
        as.list() %>%
        discard(is.na)

      tryCatch(
        expr(ifelse(!is.na(!!x), glue(!!x), NA_character_)) %>%
          eval_tidy(data = lst_env_for_eval),
        error = function(e) {
          # if string has open and close {}, then print detailed message
          if (!is_empty(lst_env_for_eval) && stringr::str_detect(x, pattern = "\\{*\\}")) {
            tryCatch(
              {
                cls <-
                  imap(lst_env_for_eval, ~ glue("{{.field {.y}}} ({{.cls {class(.x)[1]}}})")) %>%
                  paste(collapse = ", ")
                cli::cli_alert_danger(
                  "There was an error processing column {.val {y}}--likely a glue syntax error."
                )
                paste("The following fields are available to insert via glue syntax:\n", cls) %>%
                  cli::cli_alert_info()
              },
              error = function(e) invisible()
            )
          }
          glue("Error processing `modify_*()` for column '{y}'.") %>% stop(call. = FALSE)
        }
      )
    }
  ) %>%
    purrr::compact()
}
