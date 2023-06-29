#' Convert gtsummary object to a gt object
#'
#' @description Function converts a gtsummary object to a `"gt_tbl"` object,
#' that is, a table created with `gt::gt()`.
#' Function is used in the background when the results are printed or knit.
#' A user can use this function if they wish to add customized formatting
#' available via the [gt package](https://gt.rstudio.com/index.html).
#'
#' @description Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#advanced}{tbl_summary vignette}
#' or
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#advanced}{tbl_regression vignette}
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
#' @return A `gt_tbl` object
#' @family gtsummary output types
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' as_gt_ex1 <-
#'   trial[c("trt", "age", "response", "grade")] %>%
#'   tbl_summary(by = trt) %>%
#'   as_gt()
#' @section Example Output:
#'
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "as_gt_ex1.png", width = "50")`
#' }}

as_gt <- function(x, include = everything(), return_calls = FALSE, ...) {
  .assert_class(x, "gtsummary")

  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # merging column specified in `x$table_styling$cols_merge` -------------------
  # UPDATE THIS WHEN `gt::cols_merge(rows=)` argument is added!
  x <- .table_styling_cols_merge(x)

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .table_styling_expr_to_row_number(x)

  # creating list of gt calls --------------------------------------------------
  gt_calls <- table_styling_to_gt_calls(x = x, ...)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_gt-lst:addl_cmds")
  gt_calls <-
    purrr::reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) {
        add_expr_after(
          calls = x,
          add_after = names(insert_expr_after[y]),
          expr = insert_expr_after[[y]],
          new_name = paste0("user_added", y)
        )
      },
      .init = gt_calls
    )

  # converting to character vector ---------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(gt_calls),
      arg_name = "include"
    )

  # user cannot omit the first 'gt' command
  include <- "gt" %>% union(include)

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) {
    return(gt_calls[include])
  }

  # taking each gt function call, concatenating them with %>% separating them
  gt_calls[include] %>%
    c(parse_expr(.get_deprecated_option("gtsummary.as_gt.addl_cmds", default = "NULL"))) %>%
    .eval_list_of_exprs()
}

# creating gt calls from table_styling -----------------------------------------
table_styling_to_gt_calls <- function(x, ...) {
  gt_calls <- list()

  # gt -------------------------------------------------------------------------
  groupname_col <-
    switch("groupname_col" %in% x$table_styling$header$column,
      "groupname_col"
    )
  caption <-
    switch(!is.null(x$table_styling$caption),
      rlang::call2(
        attr(x$table_styling$caption, "text_interpret"),
        x$table_styling$caption
      )
    )
  gt_calls[["gt"]] <-
    expr(gt::gt(
      data = x$table_body,
      groupname_col = !!groupname_col,
      caption = !!caption,
      !!!list(...)
    ))

  # fmt_missing ----------------------------------------------------------------
  gt_calls[["fmt_missing"]] <-
    expr(
      gt::sub_missing(columns = gt::everything(), missing_text = "")
    ) %>%
    c(
      map(
        seq_len(nrow(x$table_styling$fmt_missing)),
        ~ expr(gt::sub_missing(
          columns = !!x$table_styling$fmt_missing$column[[.x]],
          rows = !!x$table_styling$fmt_missing$row_numbers[[.x]],
          missing_text = !!x$table_styling$fmt_missing$symbol[[.x]]
        ))
      )
    )

  # cols_align -----------------------------------------------------------------
  df_cols_align <-
    x$table_styling$header %>%
    select("column", "align") %>%
    group_by(.data$align) %>%
    nest() %>%
    mutate(cols = map(.data$data, ~ pull(.x, column)))

  gt_calls[["cols_align"]] <-
    map(
      seq_len(nrow(df_cols_align)),
      ~ expr(gt::cols_align(
        columns = !!df_cols_align$cols[[.x]],
        align = !!df_cols_align$align[[.x]]
      ))
    )

  # indent ---------------------------------------------------------------------
  df_indent <- x$table_styling$text_format %>% filter(.data$format_type == "indent")
  gt_calls[["indent"]] <-
    map(
      seq_len(nrow(df_indent)),
      ~ expr(gt::text_transform(
        locations = gt::cells_body(
          columns = !!df_indent$column[[.x]],
          rows = !!df_indent$row_numbers[[.x]]
        ),
        fn = function(x) paste0("\U00A0\U00A0\U00A0\U00A0", x)
      ))
    )

  # indent2 --------------------------------------------------------------------
  df_indent2 <- x$table_styling$text_format %>% filter(.data$format_type == "indent2")
  gt_calls[["indent2"]] <-
    map(
      seq_len(nrow(df_indent2)),
      ~ expr(gt::text_transform(
        locations = gt::cells_body(
          columns = !!df_indent2$column[[.x]],
          rows = !!df_indent2$row_numbers[[.x]]
        ),
        fn = function(x) paste0("\U00A0\U00A0\U00A0\U00A0\U00A0\U00A0\U00A0\U00A0", x)
      ))
    )

  # fmt ------------------------------------------------------------------------
  gt_calls[["fmt"]] <-
    map(
      seq_len(nrow(x$table_styling$fmt_fun)),
      ~ expr(gt::fmt(
        columns = !!x$table_styling$fmt_fun$column[[.x]],
        rows = !!x$table_styling$fmt_fun$row_numbers[[.x]],
        fns = !!x$table_styling$fmt_fun$fmt_fun[[.x]]
      ))
    )

  # tab_style_bold -------------------------------------------------------------
  df_bold <- x$table_styling$text_format %>% filter(.data$format_type == "bold")
  gt_calls[["tab_style_bold"]] <-
    map(
      seq_len(nrow(df_bold)),
      ~ expr(gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          columns = !!df_bold$column[[.x]],
          rows = !!df_bold$row_numbers[[.x]]
        )
      ))
    )

  # tab_style_italic -----------------------------------------------------------
  df_italic <- x$table_styling$text_format %>% filter(.data$format_type == "italic")
  gt_calls[["tab_style_italic"]] <-
    map(
      seq_len(nrow(df_italic)),
      ~ expr(gt::tab_style(
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_body(
          columns = !!df_italic$column[[.x]],
          rows = !!df_italic$row_numbers[[.x]]
        )
      ))
    )

  # cols_label -----------------------------------------------------------------
  gt_calls[["cols_label"]] <-
    map2(
      x$table_styling$header$interpret_label,
      x$table_styling$header$label,
      ~ call2(parse_expr(.x), .y)
    ) %>%
    set_names(x$table_styling$header$column) %>%
    {
      call2(expr(gt::cols_label), !!!.)
    }

  # tab_footnote ---------------------------------------------------------------
  if (nrow(x$table_styling$footnote) == 0 &&
    nrow(x$table_styling$footnote_abbrev) == 0) {
    gt_calls[["tab_footnote"]] <- list()
  } else {
    df_footnotes <-
      bind_rows(
        x$table_styling$footnote,
        x$table_styling$footnote_abbrev
      ) %>%
      nest(data = c("column", "row_numbers")) %>%
      rowwise() %>%
      mutate(
        columns = .data$data %>% pull("column") %>% unique() %>% list(),
        rows = .data$data %>% pull("row_numbers") %>% unique() %>% list()
      ) %>%
      ungroup()
    df_footnotes$footnote_exp <-
      map2(
        df_footnotes$text_interpret,
        df_footnotes$footnote,
        ~ call2(parse_expr(.x), .y)
      )


    gt_calls[["tab_footnote"]] <-
      pmap(
        list(
          df_footnotes$tab_location, df_footnotes$footnote_exp,
          df_footnotes$columns, df_footnotes$rows
        ),
        function(tab_location, footnote, columns, rows) {
          if (tab_location == "header") {
            return(expr(
              gt::tab_footnote(
                footnote = !!footnote,
                locations = gt::cells_column_labels(columns = !!columns)
              )
            ))
          }
          if (tab_location == "body") {
            return(expr(
              gt::tab_footnote(
                footnote = !!footnote,
                locations = gt::cells_body(columns = !!columns, rows = !!rows)
              )
            ))
          }
        }
      )
  }

  # spanning_header ------------------------------------------------------------
  df_spanning_header <-
    x$table_styling$header %>%
    select("column", "interpret_spanning_header", "spanning_header") %>%
    filter(!is.na(.data$spanning_header)) %>%
    nest(cols = "column") %>%
    mutate(
      spanning_header = map2(
        .data$interpret_spanning_header, .data$spanning_header,
        ~ call2(parse_expr(.x), .y)
      ),
      cols = map(.data$cols, ~ pull(.x))
    ) %>%
    select("spanning_header", "cols")

  gt_calls[["tab_spanner"]] <-
    map(
      seq_len(nrow(df_spanning_header)),
      ~ expr(gt::tab_spanner(
        columns = !!df_spanning_header$cols[[.x]],
        label = gt::md(!!df_spanning_header$spanning_header[[.x]]),
        gather = FALSE
      ))
    )

  # horizontal_line ------------------------------------------------------------
  if (!is.null(x$table_styling$horizontal_line_above)) {
    gt_calls[["horizontal_line"]] <-
      expr(
        gt::tab_style(
          style = gt::cell_borders(sides = "top", color = "#D3D3D3", weight = gt::px(2)),
          locations = gt::cells_body(rows = !!x$table_styling$horizontal_line_above)
        )
      )
  }

  # tab_source_note  -----------------------------------------------------------
  # adding other calls from x$table_styling$source_note
  if (!is.null(x$table_styling$source_note)) {
    source_note <-
      rlang::call2(attr(x$table_styling$source_note, "text_interpret"), x$table_styling$source_note)
    gt_calls[["tab_source_note"]] <- expr(gt::tab_source_note(source_note = !!source_note))
  }

  # cols_hide ------------------------------------------------------------------
  gt_calls[["cols_hide"]] <-
    names(x$table_body) %>%
    setdiff(.cols_to_show(x)) %>%
    {
      .purrr_when(
        rlang::is_empty(.) ~ NULL,
        TRUE ~ expr(gt::cols_hide(columns = !!.))
      )
    }

  # return list of gt expressions
  gt_calls
}
