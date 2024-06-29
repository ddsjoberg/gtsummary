#' Convert gtsummary object to a huxtable object
#'
#' Function converts a gtsummary object to a huxtable object.
#' A user can use this function if they wish to add customized formatting
#' available via the huxtable functions. The huxtable package supports output
#' to PDF via LaTeX, as well as HTML and Word.
#'
#' @section Excel Output:
#'
#' Use the `as_hux_xlsx()` function to save a copy of the table in an excel file.
#' The file is saved using `huxtable::quick_xlsx()`.
#'
#' @inheritParams as_flex_table
#' @inheritParams huxtable::quick_xlsx
#' @param bold_header_rows (scalar `logical`)\cr
#'   logical indicating whether to bold header rows. Default is `TRUE`
#' @param strip_md_bold `r lifecycle::badge("deprecated")`
#'
#' @name as_hux_table
#' @return A \{huxtable\} object
#'
#' @author David Hugh-Jones, Daniel D. Sjoberg
#' @examplesIf gtsummary:::is_pkg_installed("huxtable", reference_pkg = "gtsummary")
#' trial |>
#'   tbl_summary(by = trt, include = c(age, grade)) |>
#'   add_p() |>
#'   as_hux_table()
NULL

#' @export
#' @rdname as_hux_table
as_hux_table <- function(x, include = everything(), return_calls = FALSE,
                         strip_md_bold = FALSE) {
  set_cli_abort_call()
  check_class(x, "gtsummary")
  check_pkg_installed("huxtable", reference_pkg = "gtsummary")
  check_scalar_logical(return_calls)

  if (!isFALSE(strip_md_bold)) {
    lifecycle::deprecate_stop(
      "1.6.0", "gtsummary::as_hux_table(strip_md_bold=)",
      details = "Markdown syntax is now recognized by the {huxtable} package."
    )
  }
  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .table_styling_expr_to_row_number(x)

  # creating list of huxtable calls -------------------------------------------
  huxtable_calls <- table_styling_to_huxtable_calls(x = x)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_hux_table.gtsummary-lst:addl_cmds")
  huxtable_calls <-
    reduce(
      .x = seq_along(insert_expr_after),
      .f = function(x, y) {
        add_expr_after(
          calls = x,
          add_after = names(insert_expr_after[y]),
          expr = insert_expr_after[[y]],
          new_name = paste0("user_added", y)
        )
      },
      .init = huxtable_calls
    )

  # converting to character vector ----------------------------------------------
  cards::process_selectors(
    data = vec_to_df(names(huxtable_calls)),
    include = {{ include }}
  )

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) {
    return(huxtable_calls[include])
  }

  .eval_list_of_exprs(huxtable_calls[include])
}

#' @export
#' @rdname as_hux_table
as_hux_xlsx <- function(x, file, include = everything(), bold_header_rows = TRUE) {
  set_cli_abort_call()
  check_class(x, "gtsummary")
  check_pkg_installed(c("huxtable", "openxlsx"), reference_pkg = "gtsummary")
  check_scalar_logical(bold_header_rows)

  # save list of expressions to run --------------------------------------------
  huxtable_calls <-
    as_hux_table(x = x, include = {{ include }}, return_calls = TRUE) |>
    utils::modifyList(list(set_left_padding = NULL))

  # construct calls to manually indent the columns -----------------------------
  # extract the indentation instructions from table_styling
  df_text_format <-
    .table_styling_expr_to_row_number(x) |>
    getElement("table_styling") |>
    getElement("indent")

  # create expressions to add indentations to `x$table_body`
  indent_exprs <-
    pmap(
      list(df_text_format$column, df_text_format$row_numbers, df_text_format$n_spaces),
      function(column, row_numbers, n_spaces) {
        indent_spaces <- strrep(" ", times = n_spaces)
        rlang::expr(
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(!!column),
              ~ ifelse(dplyr::row_number() %in% !!row_numbers,
                       paste0(!!indent_spaces, .), .
              )
            )
          )
        )
      }
    )

  # insert indentation code before 'as_huxtable()' call ------------------------
  index_n <- which(names(huxtable_calls) %in% "huxtable")
  huxtable_calls <- append(
    x = huxtable_calls,
    values = list("indent" = indent_exprs),
    after = index_n - 1L
  )

  # bold header rows -----------------------------------------------------------
  if (isTRUE(bold_header_rows)) {
    huxtable_calls[["bold_header_rows"]] <-
      expr(huxtable::style_header_rows(bold = TRUE))
  }

  # run hux commands and export to excel ---------------------------------------
  .eval_list_of_exprs(huxtable_calls) %>%
    huxtable::quick_xlsx(file = file, open = FALSE)
}

# creating huxtable calls from table_styling -----------------------------------
table_styling_to_huxtable_calls <- function(x, ...) {
  # adding id number for columns not hidden
  x$table_styling$header <-
    x$table_styling$header %>%
    dplyr::group_by(.data$hide) %>%
    dplyr::mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) %>%
    dplyr::ungroup()

  # tibble ---------------------------------------------------------------------
  # huxtable doesn't use the markdown language `__` or `**`
  # to bold and italicize text, so removing them here
  huxtable_calls <- table_styling_to_tibble_calls(x, col_labels = FALSE)
  huxtable_calls$tab_style_bold <-
    huxtable_calls$tab_style_italic <-
    huxtable_calls$fmt_missing <- NULL

  huxtable_calls[["huxtable"]] <- expr(huxtable::as_huxtable(add_colnames = FALSE))

  # set_caption ----------------------------------------------------------------
  if (!is.null(x$table_styling$caption)) {
    huxtable_calls[["set_caption"]] <- expr(
      huxtable::set_caption(value = !!x$table_styling$caption)
    )
  }

  # padding --------------------------------------------------------------------
  df_padding <-
    x$table_styling$header %>%
    dplyr::select("id", "column") %>%
    dplyr::inner_join(
      x$table_styling$indent,
      by = "column"
    )

  huxtable_calls[["set_left_padding"]] <-
    map(
      seq_len(nrow(df_padding)),
      ~ expr(huxtable::set_left_padding(
        row = !!df_padding$row_numbers[[.x]],
        col = !!df_padding$id[[.x]],
        value = !!(df_padding$n_spaces[[.x]] * 15 / 4)
      ))
    )

  # footnote -------------------------------------------------------------------
  vct_footnote <-
    .number_footnotes(x) %>%
    dplyr::pull("footnote") %>%
    unique()
  border <- rep_len(0, length(vct_footnote))
  border[1] <- 0.8

  if (length(vct_footnote) > 0) {
    huxtable_calls[["add_footnote"]] <-
      map2(
        vct_footnote, border,
        ~ expr(
          huxtable::add_footnote(
            text = !!.x,
            border = !!.y
          )
        )
      )
  }

  # source note ----------------------------------------------------------------
  if (!is.null(x$table_styling$source_note)) {
    huxtable_calls[["add_footnote"]] <- append(
      huxtable_calls[["add_footnote"]],
      expr(
        huxtable::add_footnote(text = !!x$table_styling$source_note)
      )
    )
  }

  # bold -----------------------------------------------------------------------
  df_bold <-
    x$table_styling$text_format %>%
    dplyr::filter(.data$format_type == "bold") %>%
    dplyr::inner_join(
      x$table_styling$header %>%
        select("column", column_id = "id"),
      by = "column"
    ) %>%
    dplyr::select("format_type", "row_numbers", "column_id")

  huxtable_calls[["set_bold"]] <-
    map(
      seq_len(nrow(df_bold)),
      ~ expr(huxtable::set_bold(
        row = !!df_bold$row_numbers[[.x]],
        col = !!df_bold$column_id[[.x]],
        value = TRUE
      ))
    )

  # italic ---------------------------------------------------------------------
  df_italic <-
    x$table_styling$text_format %>%
    dplyr::filter(.data$format_type == "italic") %>%
    dplyr::inner_join(
      x$table_styling$header %>%
        select("column", column_id = "id"),
      by = "column"
    ) %>%
    dplyr::select("format_type", "row_numbers", "column_id")

  huxtable_calls[["set_italic"]] <-
    map(
      seq_len(nrow(df_italic)),
      ~ expr(huxtable::set_italic(
        row = !!df_italic$row_numbers[[.x]],
        col = !!df_italic$column_id[[.x]],
        value = TRUE
      ))
    )

  # horizontal_line_above ------------------------------------------------------
  if (!is.null(x$table_styling$horizontal_line_above)) {
    row_number <-
      eval_tidy(x$table_styling$horizontal_line_above, data = x$table_body) %>%
      which()
    huxtable_calls[["horizontal_line"]] <-
      expr(
        huxtable::set_top_border(row = !!row_number, value = 0.4)
      )
  }

  # set_na_string -------------------------------------------------------
  df_fmt_missing <-
    x$table_styling$fmt_missing %>%
    dplyr::inner_join(
      x$table_styling$header %>%
        select("column", column_id = "id"),
      by = "column"
    ) %>%
    dplyr::select("symbol", "row_numbers", "column_id") %>%
    tidyr::nest(location_ids = "column_id") %>%
    dplyr::mutate(
      column_id = map(.data$location_ids, ~ getElement(.x, "column_id") %>% unique())
    )

  huxtable_calls[["fmt_missing"]] <-
    map(
      seq_len(nrow(df_fmt_missing)),
      ~ expr(
        huxtable::set_na_string(
          row = !!df_fmt_missing$row_numbers[[.x]],
          col = !!df_fmt_missing$column_id[[.x]],
          value = !!df_fmt_missing$symbol[[.x]]
        )
      )
    )

  # insert_row ----------------------------------------------------------
  # we do this last so as to not mess up row indexes before
  col_labels <-
    x$table_styling$header %>%
    dplyr::filter(.data$hide == FALSE) %>%
    dplyr::select("column", "label") %>%
    deframe()

  huxtable_calls[["insert_row"]] <- list()

  huxtable_calls[["insert_row"]] <- append(
    huxtable_calls[["insert_row"]],
    expr(huxtable::insert_row(after = 0, !!!col_labels))
  )

  any_spanning_header <- sum(!is.na(x$table_styling$header$spanning_header)) > 0
  if (any_spanning_header) {
    header_content <- x$table_styling$header$spanning_header[x$table_styling$header$hide == FALSE]
    huxtable_calls[["insert_row"]] <- append(
      huxtable_calls[["insert_row"]],
      expr(huxtable::insert_row(after = 0, !!!header_content))
    )

    header_colspans <- rle(header_content)$lengths
    header_colspan_cols <- cumsum(c(
      1,
      header_colspans[-length(header_colspans)]
    ))
    huxtable_calls[["insert_row"]] <- append(
      huxtable_calls[["insert_row"]],
      expr(
        huxtable::set_colspan(
          row = 1, col = !!header_colspan_cols,
          value = !!header_colspans
        )
      )
    )
  }
  header_bottom_row <- if (any_spanning_header) 2 else 1
  huxtable_calls[["insert_row"]] <- append(
    huxtable_calls[["insert_row"]],
    expr(
      huxtable::set_bottom_border(
        row = !!header_bottom_row, col =
          huxtable::everywhere, value = 0.4
      )
    )
  )

  # set_markdown ---------------------------------------------------------------
  header_rows <- switch(any_spanning_header, 1:2) %||% 1L # styler: off
  huxtable_calls[["set_markdown"]] <-
    list(
      set_markdown =
        expr(huxtable::set_markdown(
          row = !!header_rows,
          col = huxtable::everywhere,
          value = TRUE
        )),
      set_header_rows = expr(huxtable::set_header_rows(row = !!header_rows, value = TRUE))
    )

  # align ----------------------------------------------------------------------
  df_align <-
    x$table_styling$header %>%
    dplyr::filter(.data$hide == FALSE) %>%
    dplyr::select("id", "align") %>%
    dplyr::group_by(.data$align) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  huxtable_calls[["align"]] <- map2(
    df_align$align, df_align$data,
    ~ expr(huxtable::set_align(
      row = huxtable::everywhere, col = !!.y$id,
      value = !!.x
    ))
  )

  # set_number_format ----------------------------------------------------------
  # this prevents huxtable from auto-formatting numbers, which are sometimes done incorrectly
  huxtable_calls[["set_number_format"]] <-
    list(set_number_format = expr(huxtable::set_number_format(NA)))

  huxtable_calls
}

