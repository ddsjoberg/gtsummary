#' Convert gtsummary object to a huxtable object
#'
#' Function converts a gtsummary object to a huxtable object.
#' A user can use this function if they wish to add customized formatting
#' available via the huxtable functions. The huxtable package supports output
#' to PDF via LaTeX, as well as HTML and Word.
#'
#' @section Details:
#' The `as_hux_table()` takes the data frame that will be printed, converts
#' it to a huxtable and formats the table with the following huxtable functions:
#'
#' 1. `huxtable::huxtable()`
#' 1. `huxtable::insert_row()` to insert header rows
#' 1. `huxtable::set_left_padding()` to indent variable levels
#' 1. `huxtable::add_footnote()` to add table footnotes and source notes
#' 1. `huxtable::set_bold()` to bold cells
#' 1. `huxtable::set_italic()` to italicize cells
#' 1. `huxtable::set_top_border()` add horizontal line (when indicated)
#' 1. `huxtable::set_na_string()` to use an em-dash for missing numbers
#' 1. `huxtable::set_markdown()` use markdown for header rows
#' 1. `huxtable::set_align()` to set column alignment
#'
#' Any one of these commands may be omitted using the `include=` argument.
#'
#' @inheritParams as_flex_table
#' @export
#' @return A {huxtable} object
#' @family gtsummary output types
#' @author David Hugh-Jones
#' @examplesIf broom.helpers::.assert_package("huxtable", boolean = TRUE)
#' trial %>%
#'   dplyr::select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   as_hux_table()
#' @export

as_hux_table <- function(x, include = everything(), return_calls = FALSE,
                         strip_md_bold = FALSE) {
  assert_package("huxtable", "as_hux_table()")

  # running pre-conversion function, if present --------------------------------
  x <- do.call(get_theme_element("pkgwide-fun:pre_conversion", default = identity), list(x))

  # converting row specifications to row numbers, and removing old cmds --------
  x <- .clean_table_styling(x)

  # stripping markdown asterisk ------------------------------------------------
  if (strip_md_bold == TRUE) {
    x$table_styling$header <-
      x$table_styling$header %>%
      mutate_at(
        vars(.data$label, .data$spanning_header),
        ~ str_replace_all(., pattern = fixed("**"), replacement = fixed(""))
      )
  }

  # creating list of huxtable calls -------------------------------------------
  huxtable_calls <- table_styling_to_huxtable_calls(x = x)

  # adding user-specified calls ------------------------------------------------
  insert_expr_after <- get_theme_element("as_hux_table.gtsummary-lst:addl_cmds")
  huxtable_calls <-
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
      .init = huxtable_calls
    )

  # converting to character vector ----------------------------------------------
  include <-
    .select_to_varnames(
      select = {{ include }},
      var_info = names(huxtable_calls),
      arg_name = "include"
    )

  # return calls, if requested -------------------------------------------------
  if (return_calls == TRUE) {
    return(huxtable_calls[include])
  }

  huxtable_calls[include] %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) expr(!!x %>% !!y)) %>%
    # evaluating expressions
    eval()
}

# creating huxtable calls from table_styling -----------------------------------
table_styling_to_huxtable_calls <- function(x, ...) {
  # adding id number for columns not hidden
  x$table_styling$header <-
    x$table_styling$header %>%
    group_by(.data$hide) %>%
    mutate(id = ifelse(.data$hide == FALSE, dplyr::row_number(), NA)) %>%
    ungroup()

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
    select(.data$id, .data$column) %>%
    inner_join(
      x$table_styling$text_format %>%
        filter(.data$format_type == "indent"),
      by = "column"
    )

  huxtable_calls[["set_left_padding"]] <-
    map(
      seq_len(nrow(df_padding)),
      ~ expr(huxtable::set_left_padding(
        row = !!df_padding$row_numbers[[.x]],
        col = !!df_padding$id[[.x]],
        value = 15
      ))
    )

  # padding2 -------------------------------------------------------------------
  df_padding2 <-
    x$table_styling$header %>%
    select(.data$id, .data$column) %>%
    inner_join(
      x$table_styling$text_format %>%
        filter(.data$format_type == "indent2"),
      by = "column"
    )

  huxtable_calls[["set_left_padding2"]] <-
    map(
      seq_len(nrow(df_padding2)),
      ~ expr(huxtable::set_left_padding(
        row = !!df_padding2$row_numbers[[.x]],
        col = !!df_padding2$id[[.x]],
        value = 30
      ))
    )

  # footnote -------------------------------------------------------------------
  vct_footnote <-
    .number_footnotes(x) %>%
    pull(.data$footnote) %>%
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
    filter(.data$format_type == "bold") %>%
    inner_join(x$table_styling$header %>%
      select(.data$column, column_id = .data$id),
    by = "column"
    ) %>%
    select(.data$format_type, .data$row_numbers, .data$column_id)

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
    filter(.data$format_type == "italic") %>%
    inner_join(x$table_styling$header %>%
      select(.data$column, column_id = .data$id),
    by = "column"
    ) %>%
    select(.data$format_type, .data$row_numbers, .data$column_id)

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
    inner_join(x$table_styling$header %>%
      select(.data$column, column_id = .data$id),
    by = "column"
    ) %>%
    select(.data$symbol, .data$row_numbers, .data$column_id) %>%
    nest(location_ids = .data$column_id) %>%
    mutate(
      column_id = map(.data$location_ids, ~ pluck(.x, "column_id") %>% unique())
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
    filter(.data$hide == FALSE) %>%
    select(.data$column, .data$label) %>%
    tibble::deframe()

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
  header_rows <- switch(any_spanning_header, 1:2) %||% 1L
  huxtable_calls[["set_markdown"]] <-
    list(set_markdown = expr(huxtable::set_markdown(row = !!header_rows,
                                                    col = huxtable::everywhere,
                                                    value = TRUE)))

  # align ----------------------------------------------------------------------
  df_align <-
    x$table_styling$header %>%
    filter(.data$hide == FALSE) %>%
    select(.data$id, .data$align) %>%
    group_by(.data$align) %>%
    nest() %>%
    ungroup()

  huxtable_calls[["align"]] <- map2(
    df_align$align, df_align$data,
    ~ expr(huxtable::set_align(
      row = huxtable::everywhere, col = !!.y$id,
      value = !!.x
    ))
  )

  huxtable_calls
}
