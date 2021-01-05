#' Convert gtsummary object to a gt object
#'
#' @description Function converts a gtsummary object to a gt_tbl object.
#' Function is used in the background when the results are printed or knit.
#' A user can use this function if they wish to add customized formatting
#' available via the [gt package](https://gt.rstudio.com/index.html).
#'
#' @description Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#advanced}{tbl_summary vignette}
#' or
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#advanced}{tbl_regression vignette}
#' for detailed examples in the 'Advanced Customization' section.
#'
#' @param x Object created by a function from the gtsummary package
#' (e.g. [tbl_summary] or [tbl_regression])
#' @param include Commands to include in output. Input may be a vector of
#' quoted or unquoted names. tidyselect and gtsummary select helper
#' functions are also accepted.
#' Default is `everything()`.
#' @param return_calls Logical. Default is `FALSE`. If `TRUE`, the calls are returned
#' as a list of expressions.
#' @param ... Arguments passed on to [gt::gt]
#' @param exclude DEPRECATED.
#' @param omit DEPRECATED.
#' @return A `gt_tbl` object
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' as_gt_ex <-
#'   trial[c("trt", "age", "response", "grade")] %>%
#'   tbl_summary(by = trt) %>%
#'   as_gt()
#' @section Example Output:
#'
#' \if{html}{\figure{as_gt_ex.png}{options: width=50\%}}

as_gt <- function(x, include = everything(), return_calls = FALSE, ...,
                  exclude = NULL, omit = NULL) {
  # making list of commands to include -----------------------------------------
  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::as_gt(exclude = )",
      "as_gt(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude commands, use the minus sign.\n",
        "For example, `include = -tab_spanner`"
      )
    )
  }

  if (!is.null(omit)) {
    lifecycle::deprecate_stop(
      "1.2.0",
      "gtsummary::as_gt(omit = )",
      "as_gt(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude commands, use the minus sign.\n",
        "For example, `include = -tab_spanner`"
      )
    )
  }

  # creating list of gt calls --------------------------------------------------
  gt_calls <- table_header_to_gt_calls(x = x, ...)
  # adding other calls from x$list_output$source_note
  if (!is.null(x$list_output$source_note)) {
    gt_calls[["tab_source_note"]] <- expr(gt::tab_source_note(source_note = !!x$list_output$source_note))
  }

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_gt-lst:addl_cmds")
  gt_calls <-
    purrr::reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) add_expr_after(calls = x,
                                         add_after = names(insert_expr_after[y]),
                                         expr = insert_expr_after[[y]],
                                         new_name = paste0("user_added", y)),
      .init = gt_calls
    )

  # converting to charcter vector ----------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(gt_calls),
      arg_name = "include"
    )
  exclude <-
    .select_to_varnames(
      select = {{ exclude }},
      var_info = names(gt_calls),
      arg_name = "exclude"
    )

  # this ensures list is in the same order as names(x$gt_calls)
  include <- names(gt_calls) %>% intersect(include)

  # user cannot omit the first 'gt' command
  include <- include %>% setdiff(exclude)
  include <- "gt" %>% union(include)

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) return(gt_calls[include])

  # taking each gt function call, concatenating them with %>% separating them
  gt_calls[include] %>%
    # adding default gt formatting options
    c(parse_expr(getOption("gtsummary.as_gt.addl_cmds", default = "NULL"))) %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval()
}

# creating gt calls from table_header ------------------------------------------
table_header_to_gt_calls <- function(x, ...) {
  table_header <- .clean_table_header(x$table_header)
  gt_calls <- list()

  # gt -------------------------------------------------------------------------
  if (!is.null(x$list_output$caption) && "caption" %in% names(as.list(gt::gt))) {
    caption <- rlang::call2(attr(x$list_output$caption, "text_interpret"), x$list_output$caption)
    gt_calls[["gt"]] <-
      expr(gt::gt(data = x$table_body, caption = !!caption, !!!list(...)))
  }
  else
    gt_calls[["gt"]] <-
    expr(gt::gt(data = x$table_body, !!!list(...)))

  # fmt_missing ----------------------------------------------------------------
  gt_calls[["fmt_missing"]] <- expr(
    gt::fmt_missing(columns = gt::everything(), missing_text = '')
  )

  # fmt_missing_emdash ---------------------------------------------------------
  df_fmt_missing_emdash <-
    table_header %>%
    filter(!is.na(.data$missing_emdash))

  gt_calls[["fmt_missing_emdash"]] <-
    map(
      seq_len(nrow(df_fmt_missing_emdash)),
      ~ expr(gt::fmt_missing(columns = gt::vars(!!!syms(df_fmt_missing_emdash$column[[.x]])),
                             rows = !!parse_expr(df_fmt_missing_emdash$missing_emdash[[.x]]),
                             missing_text = !!get_theme_element("tbl_regression-str:ref_row_text",
                                                              default = "---")))
    )

  # cols_align -----------------------------------------------------------------
  df_cols_align <-
    table_header %>%
    select(.data$column, .data$align) %>%
    group_by(.data$align) %>%
    nest() %>%
    mutate(cols = map(.data$data, ~ pull(.x, column)))

  gt_calls[["cols_align"]] <-
    map(
      seq_len(nrow(df_cols_align)),
      ~ expr(gt::cols_align(columns = gt::vars(!!!syms(df_cols_align$cols[[.x]])),
                            align = !!df_cols_align$align[[.x]]))
    )

  # indent ---------------------------------------------------------------------
  df_tab_style_indent <-
    table_header %>%
    filter(!is.na(.data$indent))

  gt_calls[["tab_style_indent"]] <-
    map(
      seq_len(nrow(df_tab_style_indent)),
      ~ expr(gt::tab_style(style = gt::cell_text(indent = gt::px(10), align = 'left'),
                           locations = gt::cells_body(columns = gt::vars(!!!syms(df_tab_style_indent$column[[.x]])),
                                                      rows = !!parse_expr(df_tab_style_indent$indent[[.x]]))))
    )

  # fmt ------------------------------------------------------------------------
  df_fmt <- table_header %>%
    filter(map_lgl(.data$fmt_fun, ~!is.null(.x)))

  gt_calls[["fmt"]] <-
    map(
      seq_len(nrow(df_fmt)),
      ~ expr(gt::fmt(columns = gt::vars(!!!syms(df_fmt$column[[.x]])),
                     rows = !is.na(!!!syms(df_fmt$column[[.x]])),
                     fns = !!df_fmt$fmt_fun[[.x]]))
    )

  # tab_style_bold -------------------------------------------------------------
  df_tab_style_bold <- table_header %>%
    filter(!is.na(.data$bold))

  gt_calls[["tab_style_bold"]] <-
    map(
      seq_len(nrow(df_tab_style_bold)),
      ~ expr(gt::tab_style(style = gt::cell_text(weight = 'bold'),
                           locations = gt::cells_body(
                             columns = gt::vars(!!!syms(df_tab_style_bold$column[[.x]])),
                             rows = !!parse_expr(df_tab_style_bold$bold[[.x]]))))
    )

  # tab_style_italic -----------------------------------------------------------
  df_tab_style_italic <- table_header %>%
    filter(!is.na(.data$italic))

  gt_calls[["tab_style_italic"]] <-
    map(
      seq_len(nrow(df_tab_style_italic)),
      ~ expr(gt::tab_style(style = gt::cell_text(style = 'italic'),
                           locations = gt::cells_body(
                             columns = gt::vars(!!!syms(df_tab_style_italic$column[[.x]])),
                             rows = !!parse_expr(df_tab_style_italic$italic[[.x]]))))
    )

  # cols_label -----------------------------------------------------------------
  # gt table_header to gt cols_label code
  df_cols_label <-
    table_header %>%
    filter(.data$hide == FALSE)

  gt_calls[["cols_label"]] <-
    map2(
      df_cols_label$text_interpret,
      df_cols_label$label,
      ~ call2(parse_expr(.x), .y)
    ) %>%
    set_names(df_cols_label$column) %>%
    {call2(expr(gt::cols_label), !!!.)}

  # tab_footnote ---------------------------------------------------------------
  df_tab_footnote <-
    table_header %>%
    filter(!is.na(.data$footnote))

  tab_footnote <-
    map(
      seq_len(nrow(df_tab_footnote)),
      ~ expr(gt::tab_footnote(
          footnote = !!df_tab_footnote$footnote[[.x]],
          locations = gt::cells_column_labels(
            columns = gt::vars(!!!syms(df_tab_footnote$column[[.x]])))))
    )

  df_tab_footnote_abbrev <-
    table_header %>%
    filter(!is.na(.data$footnote_abbrev))

  if (nrow(df_tab_footnote_abbrev) == 0) tab_footnote_abbrev <- NULL
  else {
    tab_footnote_abbrev <-
      expr(gt::tab_footnote(
        footnote = !!paste(unique(df_tab_footnote_abbrev$footnote_abbrev), collapse = ", "),
        locations = gt::cells_column_labels(columns = gt::vars(!!!syms(df_tab_footnote_abbrev$column)))
      ))
  }

  gt_calls[["tab_footnote"]] <- c(tab_footnote, tab_footnote_abbrev)

  # spanning_header ------------------------------------------------------------
  df_spanning_header <-
    table_header %>%
    filter(!is.na(.data$spanning_header)) %>%
    select(.data$column, .data$spanning_header) %>%
    group_by(.data$spanning_header) %>%
    nest() %>%
    mutate(cols = map(.data$data, ~ pull(.x, column)))

  gt_calls[["tab_spanner"]] <-
    map(
      seq_len(nrow(df_spanning_header)),
      ~ expr(gt::tab_spanner(columns = gt::vars(!!!syms(df_spanning_header$cols[[.x]])),
                            label = gt::md(!!df_spanning_header$spanning_header[[.x]])))
    )

  # cols_hide ------------------------------------------------------------------
  gt_calls[["cols_hide"]] <-
    table_header %>%
    filter(.data$hide == TRUE) %>%
    pull(.data$column) %>%
    {expr(gt::cols_hide(columns = gt::vars(!!!syms(.))))}

  # return list of gt expressions
  gt_calls
}

# this function cleans up table_header (i.e. removes formatting for hidden columns, etc.)
.clean_table_header <- function(x) {
  # removing instructions for hidden columns
  dplyr::mutate_at(
    x,
    vars(any_of(c("bold", "italic", "missing_emdash", "indent", "footnote_abbrev", "footnote"))),
    ~ifelse(.data$hide, NA_character_, .)
  )
}


# converts the columns in
.convert_header_to_rows <- function(x) {
  # convert columns that use row and values to format --------------------------
  x %>%
    .convert_header_to_rows_one_column("fmt_fun") %>%
    .convert_header_to_rows_one_column("footnote") %>%
    .convert_header_to_rows_one_column("bold") %>%
    .convert_header_to_rows_one_column("italic") %>%
    .convert_header_to_rows_one_column("missing_emdash")
}


.convert_header_to_rows_one_column <- function(x, column) {
  if (!column %in% names(x$table_header)) return(x)

  if (column %in% c("fmt_fun", "footnote")) {
    table_rows_update <-
      x$table_header %>%
      select(.data$column, all_of(.env$column)) %>%
      rlang::set_names(c("column", "format_value")) %>%
      mutate(
        format_type = .env$column,
        rows = NA_character_,
        format_value = map(.data$format_value, ~switch(!is_empty(na.omit(.x)), .x))
      ) %>%
      select(.data$column, .data$format_type, .data$rows, .data$format_value) %>%
      filter(!purrr::map_lgl(format_value, is.null))
  }
  else if (column %in% c("bold", "italic", "missing_emdash")) {
    table_rows_update <-
      x$table_header %>%
      select(.data$column, all_of(.env$column)) %>%
      rlang::set_names(c("column", "rows")) %>%
      mutate(
        format_type = .env$column,
        format_value = list(NULL)
      ) %>%
      select(.data$column, .data$format_type, .data$rows, .data$format_value) %>%
      filter(!is.na(.data$rows))
  }

  # remove column from table_header --------------------------------------------
  x$table_header <- x$table_header %>% select(-all_of(.env$column))

  # combining into a new table_rows tibble -------------------------------------
  x$table_rows <- bind_rows(x$table_rows, table_rows_update)
  x
}
