#' Modify Table Styling
#'
#' This is a function meant for advanced users to gain
#' more control over the characteristics of the resulting
#' gtsummary table by directly modifying `.$table_styling`.
#' *This function is primarily used in the development of other gtsummary
#' functions, and very little checking of the passed arguments is performed.*
#'
#' Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary definition}
#' vignette for information on `.$table_styling` objects.
#'
#' @param x (`gtsummary`)\cr
#'   gtsummary object
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Selector of columns in `x$table_body`
#' @param rows (predicate `expression`)\cr
#'   Predicate expression to select rows in `x$table_body`.
#'   Can be used to style footnote, formatting functions, missing symbols,
#'   and text formatting. Default is `NULL`. See details below.
#' @param label (`character`)\cr
#'   Character vector of column label(s). Must be the same length as `columns`.
#' @param hide (scalar `logical`)
#'   Logical indicating whether to hide column from output
#' @param align (`string`)
#'   String indicating alignment of column, must be one of `c("left", "right", "center")`
#' @param text_format,undo_text_format (`string`)\cr
#'   String indicated which type of text formatting to apply/remove to the rows and columns.
#'   Must be one of `c("bold", "italic")`.
#' @param text_interpret (`string`)\cr
#'   Must be one of `"md"` or `"html"` and indicates the processing function
#'   as `gt::md()` or `gt::html()`. Use this in conjunction with arguments for
#'   header and footnotes.
#' @param fmt_fun (`function`)\cr
#'   function that formats the statistics in the columns/rows in `columns` and `rows`
#' @param footnote_abbrev (`string`)\cr
#'   string with abbreviation definition, e.g. `"CI = Confidence Interval"`
#' @param footnote (`string`)\cr
#'   string with text for footnote
#' @param spanning_header (`string`)\cr
#'   string with text for spanning header
#' @param missing_symbol (`string`)\cr
#'   string indicating how missing values are formatted.
#' @param cols_merge_pattern (`string`) \lifecycle{experimental}\cr
#'   glue-syntax string indicating how to merge
#'   columns in `x$table_body`. For example, to construct a confidence interval
#'   use `"{conf.low}, {conf.high}"`. The first column listed in the pattern
#'   string must match the single column name passed in `columns=`.
#' @param indent (`integer`)\cr
#'  An integer indicating how many space to indent text
#'
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary internals vignette}
#'
#' @export
#' @family Advanced modifiers
#'
#' @section rows argument:
#' The rows argument accepts a predicate expression that is used to specify
#' rows to apply formatting. The expression must evaluate to a logical when
#' evaluated in `x$table_body`. For example, to apply formatting to the age rows
#' pass `rows = variable == "age"`. A vector of row numbers is NOT acceptable.
#'
#' A couple of things to note when using the `rows` argument.
#' 1. You can use saved objects to create the predicate argument, e.g.
#'   `rows = variable == letters[1]`.
#' 2. The saved object cannot share a name with a column in `x$table_body`.
#'   The reason for this is that in `tbl_merge()` the columns are renamed,
#'   and the renaming process cannot disambiguate the `variable` column from
#'   an external object named `variable` in the following expression
#'   `rows = .data$variable = .env$variable`.
#'
#' @section cols_merge_pattern argument:
#'
#' There are planned updates to the implementation of column merging.
#' Currently, this function replaces the numeric column with a
#' formatted character column following `cols_merge_pattern=`.
#' Once `gt::cols_merge()` gains the `rows=` argument the
#' implementation will be updated to use it, which will keep
#' numeric columns numeric. For the _vast majority_ of users,
#' _the planned change will be go unnoticed_.
#'
#' If this functionality is used in conjunction with `tbl_stack()` (which
#' includes `tbl_uvregression()`), there is potential issue with printing.
#' When columns are stack AND when the column-merging is
#' defined with a quosure, you may run into issues due to the loss of the
#' environment when 2 or more quosures are combined. If the expression
#' version of the quosure is the same as the quosure (i.e. no evaluated
#' objects), there should be no issues. Regardless, this argument is used
#' internally with care, and it is _not_ recommended for users.
modify_table_styling <- function(x,
                                 columns,
                                 rows = NULL,
                                 label = NULL,
                                 spanning_header = NULL,
                                 hide = NULL,
                                 footnote = NULL,
                                 footnote_abbrev = NULL,
                                 align = NULL,
                                 missing_symbol = NULL,
                                 fmt_fun = NULL,
                                 text_format = NULL,
                                 undo_text_format = NULL,
                                 indent = NULL,
                                 text_interpret = c("md", "html"),
                                 cols_merge_pattern = NULL) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_table_styling = match.call()))
  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")

  # update table_styling -------------------------------------------------------
  x <- .update_table_styling(x)

  # convert column input to string ---------------------------------------------
  cards::process_selectors(
    data = scope_header(x$table_body, x$table_styling$header),
    columns = {{ columns }}
  )

  # if no columns selected, returning unaltered
  if (is_empty(columns)) {
    return(x)
  }

  # deprecation ----------------------------------------------------------------
  .check_ref_to_ci_column(x, columns, cols_merge_pattern)
  if (isFALSE(undo_text_format)) undo_text_format <- NULL
  if (isTRUE(undo_text_format)) {
    # set new values for the user
    if (any(c("indent", "indent2") %in% text_format)) {
      indent <- 0L
    }
    undo_text_format <- text_format |> setdiff(c("indent", "indent2"))
    text_format <- NULL

    updated_call <-
      match.call() |>
      as.list() |>
      utils::modifyList(
        list(text_format = NULL, undo_text_format = undo_text_format, indent = indent)
      ) |>
      compact()
    if (nchar(paste(expr_deparse(updated_call[["x"]]), collapse = "")) > 30L) updated_call[["x"]] <- expr(.)
    updated_call <- as.call(updated_call) |> expr_deparse(width = Inf)

    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::modify_table_styling(undo_text_format = 'must be one of \"bold\" or \"italic\"')",
      details = glue::glue("Update function call to `{updated_call}`.")
    )
  }
  if (any(c("indent", "indent2") %in% text_format)) {
    lst_new_args <-
      list(
        indent = ifelse("indent" %in% text_format, 4L, 8L),
        text_format =
          text_format |>
            setdiff(c("indent", "indent2")) %>%
            {.ifelse1(is_empty(.), NULL, .)} # styler: off
      )
    env_bind(.env = current_env(), !!!lst_new_args)

    updated_call <-
      match.call() |>
      as.list() |>
      utils::modifyList(lst_new_args) |>
      compact()
    if (nchar(paste(expr_deparse(updated_call[["x"]]), collapse = "")) > 30) updated_call[["x"]] <- expr(.)
    updated_call <- as.call(updated_call) |> expr_deparse(width = Inf)

    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::modify_table_styling(text_format = 'must be one of \"bold\" or \"italic\"')",
      details = glue::glue("Update function call to `{updated_call}`.")
    )
  }

  text_interpret <- paste0("gt::", arg_match(text_interpret))

  if (!is_empty(text_format)) {
    text_format <- arg_match(text_format, values = c("bold", "italic"), multiple = TRUE)
  }
  if (!is_empty(undo_text_format)) {
    undo_text_format <- arg_match(undo_text_format, values = c("bold", "italic"), multiple = TRUE)
  }
  rows <- enquo(rows)
  rows_eval_error <-
    tryCatch(
      eval_tidy(rows, data = x$table_body) %>%
        {!is.null(.) && !is.logical(.)}, # styler: off
      error = function(e) TRUE
    )
  if (rows_eval_error) {
    cli::cli_abort(
      "The {.arg rows} argument must be an expression that evaluates to a logical vector in {.code x$table_body}.",
      call = get_cli_abort_call()
    )
  }

  # label ----------------------------------------------------------------------
  if (!is_empty(label)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        dplyr::tibble(column = columns, interpret_label = text_interpret, label = label),
        by = "column"
      )
  }

  # spanning_header ------------------------------------------------------------
  if (!is_empty(spanning_header)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        dplyr::tibble(column = columns, interpret_spanning_header = text_interpret, spanning_header = spanning_header),
        by = "column"
      )
  }

  # hide -----------------------------------------------------------------------
  if (!is_empty(hide)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        dplyr::tibble(column = columns, hide = hide),
        by = "column"
      )
  }

  # align ----------------------------------------------------------------------
  if (!is_empty(align)) {
    x$table_styling$header <-
      x$table_styling$header %>%
      dplyr::rows_update(
        dplyr::tibble(column = columns, align = align),
        by = "column"
      )
  }

  # footnote -------------------------------------------------------------------
  if (!is_empty(footnote)) {
    x$table_styling$footnote <-
      dplyr::bind_rows(
        x$table_styling$footnote,
        dplyr::tibble(
          column = columns,
          rows = list(rows),
          text_interpret = text_interpret,
          footnote = footnote
        )
      )
  }

  # footnote_abbrev ------------------------------------------------------------
  if (!is_empty(footnote_abbrev)) {
    x$table_styling$footnote_abbrev <-
      dplyr::bind_rows(
        x$table_styling$footnote_abbrev,
        dplyr::tibble(
          column = columns,
          rows = list(rows),
          text_interpret = text_interpret,
          footnote = footnote_abbrev
        )
      )
  }

  # fmt_fun --------------------------------------------------------------------
  if (!is_empty(fmt_fun)) {
    if (rlang::is_function(fmt_fun)) fmt_fun <- list(fmt_fun)
    x$table_styling$fmt_fun <-
      dplyr::bind_rows(
        x$table_styling$fmt_fun,
        dplyr::tibble(
          column = columns,
          rows = list(rows),
          fmt_fun = fmt_fun
        )
      )
  }

  # text_format ----------------------------------------------------------------
  if (!is_empty(text_format)) {
    x$table_styling$text_format <-
      list(
        column = columns,
        rows = list(rows),
        format_type = text_format,
        undo_text_format = FALSE
      ) %>%
      {tidyr::expand_grid(!!!.)} %>% # styler: off
      {dplyr::bind_rows(x$table_styling$text_format, .)} # styler: off
  }
  if (!is_empty(undo_text_format)) {
    x$table_styling$text_format <-
      list(
        column = columns,
        rows = list(rows),
        format_type = undo_text_format,
        undo_text_format = TRUE
      ) %>%
      {tidyr::expand_grid(!!!.)} %>% # styler: off
      {dplyr::bind_rows(x$table_styling$text_format, .)} # styler: off
  }

  # indent ---------------------------------------------------------------------
  if (!is_empty(indent)) {
    if (!is_scalar_integerish(indent) || indent < 0L) {
      cli::cli_abort("The {.arg indent} argument must be a non-negative scalar integer.")
    }
    x$table_styling$indent <-
      dplyr::bind_rows(
        x$table_styling$indent,
        dplyr::tibble(
          column = columns,
          rows = list(rows),
          n_spaces = as.integer(indent)
        )
      )
  }

  # missing_symbol -------------------------------------------------------------
  if (!is_empty(missing_symbol)) {
    x$table_styling$fmt_missing <-
      list(
        column = columns,
        rows = list(rows),
        symbol = missing_symbol
      ) %>%
      {tidyr::expand_grid(!!!.)} %>% # styler: off
      {dplyr::bind_rows(x$table_styling$fmt_missing, .)} # styler: off
  }

  # cols_merge_pattern ---------------------------------------------------------
  if (!is_empty(cols_merge_pattern)) {
    if (!is.na(cols_merge_pattern)) check_string(cols_merge_pattern)
    x <-
      .modify_cols_merge(
        x,
        column = columns,
        rows = !!rows,
        pattern = cols_merge_pattern
      )
  }

  # return x -------------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

# use of the "ci" column was deprecated in v2.0
.check_ref_to_ci_column <- function(x, columns, cols_merge_pattern) {
  # if "ci" column was selected & selector was not `everything()`, then print note
  if (("ci" %in% columns && !all(names(x$table_body) %in% columns)) ||
      (!is_empty(cols_merge_pattern) && "ci" %in% .extract_glue_elements(cols_merge_pattern))) {
    cli::cli_warn(
      c("Use of the {.val ci} column was deprecated in {.pkg gtsummary} v2.0,
         and the column will eventually be removed from the tables.",
        "!" = "Review {.help deprecated_ci_column} for details on {.emph how to update your code}.\n\n",
        i = "The {.val ci} column has been replaced by the merged {.val {c('conf.low', 'conf.high')}} columns (merged with {.fun modify_column_merge}).",
        i = "In most cases, a simple update from {.code ci = 'a new label'} to {.code conf.low = 'a new label'} is sufficient.")
    )
  }
}


# function to add merging columns instructions
.modify_cols_merge <- function(x, column, rows, pattern) {
  rows <- enquo(rows)
  all_columns <- .extract_glue_elements(pattern)

  if (!is_empty(pattern) && !all(all_columns %in% x$table_styling$header$column)) {
    cli::cli_abort(
      c("All columns specified in {.arg cols_merge_pattern} argument must be present in {.code x$table_body}",
        "i" = "The following columns are not present: {.val {setdiff(all_columns, x$table_styling$header$column)}}"),
      call = get_cli_abort_call()
    )
  }

  if (!is.na(pattern) && !identical(column, all_columns[1])) {
    cli::cli_abort(
      c("A single column must be specified in the {.arg columns} argument when
         using {.arg cols_merge_pattern}, and that column must be the first to
         appear in the pattern argument.",
        i = "For example, {.code modify_table_styling(columns={.val {all_columns[1]}}, cols_merge_pattern={.val {pattern}})}"),
      call = get_cli_abort_call()
    )
  }

  x$table_styling$cols_merge <-
    x$table_styling$cols_merge %>%
    # remove previous merging for specified column
    dplyr::filter(!.data$column %in% .env$column) %>%
    # append new merge instructions
    dplyr::bind_rows(
      dplyr::tibble(
        column = column,
        rows = list(rows),
        pattern = pattern
      )
    )

  # hiding all but the first column
  x <- modify_column_hide(x, columns = all_columns[-1])

  # return gtsummary table
  x
}
