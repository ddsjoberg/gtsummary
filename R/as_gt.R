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

  # print message if old printing style object exists --------------------------
  if (!is.null(x$table_header)) {
    if (!is.null(x$table_body_styling))
      paste("'gtsummary' object was created with code from with <v1.4.0 and >=v1.4.0.",
            "Unexpected formatting may occur.") %>%
      str_wrap() %>%
      ui_info()
    x <- .convert_table_header_to_styling(x)
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
  gt_calls[["fmt_missing"]] <-
    c(gt_calls[["fmt_missing"]],
      map(
        seq_len(nrow(x$table_body_styling$fmt_missing)),
        ~ expr(gt::fmt_missing(columns = gt::vars(!!!syms(x$table_body_styling$fmt_missing$column[[.x]])),
                               rows = !!parse_expr(x$table_body_styling$fmt_missing$rows[[.x]]),
                               missing_text = !!x$table_body_styling$fmt_missing$symbol[[.x]]))
      )
    )

  # cols_align -----------------------------------------------------------------
  df_cols_align <-
    x$table_body_styling$align %>%
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
  gt_calls[["tab_style_indent"]] <-
    map(
      seq_len(nrow(x$table_body_styling$indent)),
      ~ expr(gt::tab_style(style = gt::cell_text(indent = gt::px(10), align = 'left'),
                           locations = gt::cells_body(columns = gt::vars(!!!syms(x$table_body_styling$indent$column[[.x]])),
                                                      rows = !!parse_expr(x$table_body_styling$indent$rows[[.x]]))))
    )

  # fmt ------------------------------------------------------------------------
  gt_calls[["fmt"]] <-
    map(
      seq_len(nrow(x$table_body_styling$fmt_fun)),
      ~ expr(gt::fmt(columns = gt::vars(!!sym(x$table_body_styling$fmt_fun$column[[.x]])),
                     rows = !!switch(!is.na(x$table_body_styling$fmt_fun$rows[[.x]]),
                                     parse_expr(x$table_body_styling$fmt_fun$rows[[.x]])),
                     fns = !!x$table_body_styling$fmt_fun$fmt_fun[[.x]]))
    )

  # tab_style_bold -------------------------------------------------------------
  gt_calls[["tab_style_bold"]] <-
    map(
      seq_len(nrow(x$table_body_styling$bold)),
      ~ expr(gt::tab_style(style = gt::cell_text(weight = 'bold'),
                           locations = gt::cells_body(
                             columns = gt::vars(!!sym(x$table_body_styling$bold$column[[.x]])),
                             rows = !!switch(!is.na(x$table_body_styling$bold$rows[[.x]]),
                                             parse_expr(x$table_body_styling$bold$rows[[.x]])))))
    )

  # tab_style_italic -----------------------------------------------------------
  gt_calls[["tab_style_italic"]] <-
    map(
      seq_len(nrow(x$table_body_styling$italic)),
      ~ expr(gt::tab_style(style = gt::cell_text(style = 'italic'),
                           locations = gt::cells_body(
                             columns = gt::vars(!!sym(x$table_body_styling$italic$column[[.x]])),
                             rows = !!switch(!is.na(x$table_body_styling$italic$rows[[.x]]),
                                             parse_expr(x$table_body_styling$italic$rows[[.x]])))))
    )

  # cols_label -----------------------------------------------------------------
  # gt table_header to gt cols_label code
  gt_calls[["cols_label"]] <-
    map2(
      x$table_body_styling$label$text_interpret,
      x$table_body_styling$label$label,
      ~ call2(parse_expr(.x), .y)
    ) %>%
    set_names(x$table_body_styling$label$column) %>%
    {call2(expr(gt::cols_label), !!!.)}

  # tab_footnote ---------------------------------------------------------------
  .to_location_expr <- function(column, row_numbers, tab_location) {
    if (tab_location == "header")
      return(expr(gt::cells_column_labels(columns = gt::vars(!!sym(column)))) %>% eval())
    if (tab_location == "body")
      return(expr(gt::cells_body(columns = gt::vars(!!sym(column)), rows = !!row_numbers)) %>% eval())
  }
  .loc_to_footnote_expr <- function(footnote, location) {
    expr(gt::tab_footnote(footnote = !!footnote, locations = eval(!!location)))
  }
  .table_styling_to_gt_footnote_call <- function(x, footnote_type) {
    if (.cols_to_show(x) %>%
        intersect(x$table_body_styling[[footnote_type]]$column) %>%
        rlang::is_empty())
      return(list())

    part1 <-
      x$table_body_styling[[footnote_type]] %>%
      rename(footnote = !!sym(footnote_type)) %>%
      filter(.data$column %in% .cols_to_show(x)) %>%
      rowwise() %>%
      mutate(
        tab_location = ifelse(is.na(rows), "header", "body"),
        row_numbers = .rows_expr_to_row_numbers(x$table_body, rows) %>% list(),
      ) %>%
      unnest(cols = .data$row_numbers) %>%
      group_by(column, tab_location, row_numbers) %>%
      dplyr::slice_tail() %>% # keeping the most recent addition
      filter(!is.na(footnote)) # keep non-missing additions
    if (nrow(part1) == 0) return(list())

    if (footnote_type == "footnote_abbrev") {
      part1$footnote <- paste(part1$footnote, collapse = ", ")
    }

    part1 %>%
      mutate(
        location_expr = pmap(list(column, row_numbers, tab_location), .to_location_expr)
      ) %>%
      ungroup() %>%
      select(footnote, text_interpret, location_expr) %>%
      nest(df_exprs = c(location_expr)) %>%
      mutate(
        location_expr_list = map(df_exprs, pluck, "location_expr"),
        footnote_expr = map2(text_interpret, footnote, ~call2(rlang::parse_expr(.x), .y)),
        final_expr =  map2(footnote_expr, location_expr_list, .loc_to_footnote_expr)
      ) %>%
      pull(.data$final_expr)
  }

  gt_calls[["tab_footnote"]] <- c(.table_styling_to_gt_footnote_call(x, "footnote"),
                                  .table_styling_to_gt_footnote_call(x, "footnote_abbrev"))

  # spanning_header ------------------------------------------------------------
  df_spanning_header <-
    x$table_body_styling$spanning_header %>%
    filter(.data$column %in% .cols_to_show(x)) %>%
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
    names(x$table_body) %>%
    setdiff(.cols_to_show(x)) %>%
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


# convert columns that use row and values to format ----------------------------
.convert_table_header_to_styling <- function(x) {
  x %>%
    .convert_header_to_rows_one_column("hide") %>%
    .convert_header_to_rows_one_column("align") %>%
    .convert_header_to_rows_one_column("label") %>%
    .convert_header_to_rows_one_column("spanning_header") %>%
    .convert_header_to_rows_one_column("footnote") %>%
    .convert_header_to_rows_one_column("footnote_abbrev") %>%
    .convert_header_to_rows_one_column("missing_emdash") %>%
    .convert_header_to_rows_one_column("indent") %>%
    .convert_header_to_rows_one_column("bold") %>%
    .convert_header_to_rows_one_column("italic") %>%
    .convert_header_to_rows_one_column("fmt_fun")
}


.convert_header_to_rows_one_column <- function(x, column) {

  if (column %in% "label") {
    x$table_body_styling[[column]] <-
      x$table_header %>%
      filter(!is.na(.data[[column]])) %>%
      select(.data$column, .data$text_interpret, .data$label)
  }
  else if (column %in% "spanning_header") {
    x$table_body_styling[[column]] <-
      x$table_header %>% select(.data$column, .data$spanning_header) %>%
      filter(!is.na(.data[[column]])) %>%
      mutate(text_interpret = "gt::md") %>%
      select(.data$column, .data$text_interpret, .data$spanning_header)
  }
  else if (column %in% c("hide", "align")) {
    x$table_body_styling[[column]] <-
      x$table_header %>% select(all_of(c("column", .env$column)))
  }
  else if (column %in% c("footnote", "footnote_abbrev")) {
    x$table_body_styling[[column]] <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      filter(!is.na(.data[[column]])) %>%
      mutate(rows = NA_character_,
             text_interpret = "gt::md") %>%
      select(all_of(c("column", "rows", "text_interpret", .env$column)))
  }
  else if (column %in% c("indent", "bold", "italic")) {
    x$table_body_styling[[column]] <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      filter(!is.na(.data[[column]])) %>%
      set_names(c("column", "rows"))
  }
  else if (column %in% "missing_emdash") {
    x$table_body_styling[["fmt_missing"]] <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      filter(!is.na(.data[[column]])) %>%
      set_names(c("column", "rows")) %>%
      mutate(symbol = "---")
  }
  else if (column %in% "fmt_fun") {
    x$table_body_styling[[column]] <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      filter(!map_lgl(.data[[column]], is.null)) %>%
      mutate(rows = NA_character_)  %>%
      select(all_of(c("column", "rows", .env$column)))
  }

  # return gtsummary table
  x
}

# takes a table_body and a character rows expression, and returns the resulting row numbers
.rows_expr_to_row_numbers <- function(table_body, rows) {
  if (is.na(rows)) return(NA)

  df <- as.data.frame(table_body) %>% tibble::remove_rownames()

  expr(subset(df, !!rlang::parse_expr(rows))) %>%
    eval() %>%
    rownames() %>%
    as.integer()
}

.cols_to_show <- function(x) {
  x$table_body_styling$hide %>%
    filter(!.data$hide) %>%
    pull(column)
}
